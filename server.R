#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(summarytools)
library(GGally)
library(corrgram)
library(naniar)
library(ggplot2)
library(car)
library(vcd)
library(visdat)
library(DT)
library(shinyjs)
library(caret)
library(rpart)
library(rpart.plot)
library(recipes)
library(glmnet)
library(ggrepel)

d <- read.csv("Data.csv", header = TRUE, na.strings =
                    c("NA", "--"), stringsAsFactors = TRUE)
d[d == -99] <- NA

d$GOVERN_TYPE <- as.character(d$GOVERN_TYPE)
d$HEALTHCARE_BASIS <- as.character(d$HEALTHCARE_BASIS) 

d$GOVERN_TYPE[is.na(d$GOVERN_TYPE)] <- "none"
d$HEALTHCARE_BASIS[is.na(d$HEALTHCARE_BASIS)] <- "none"

d$GOVERN_TYPE <- as.factor(d$GOVERN_TYPE)
d$HEALTHCARE_BASIS <- as.factor(d$HEALTHCARE_BASIS)

d$HEALTHCARE_COST[is.na(d$HEALTHCARE_COST) & d$HEALTHCARE_BASIS=="FREE"] <- 0

d$CODE <- as.integer(substring(d$CODE, 8))

d$MISSINGNESS <- apply(X = is.na(d), MARGIN = 1,FUN = sum)

d_shadow <- is.na(d) + 0
cm <- colMeans(d_shadow)
d_shadow <- d_shadow[, cm > 0 & cm < 1, drop = FALSE]




shinyServer(function(input, output, session) {
    
    getClean <- reactive({
        pMiss <- function(x){ sum(is.na(x))/length(x)*100 }
        vRatio <- apply(d,2,pMiss)
        d <- d[, vRatio < input$vthreshold]
        oRatio <- apply(d,1,pMiss)
        d <- d[oRatio < input$othreshold, ]
        d
      })
    
    getTrain <- reactive({
      d_train <- getClean()[getClean()$OBS_TYPE == "Train",]
      d_train
    })
    
    getTest <- reactive({
      d_test <- getClean()[getClean()$OBS_TYPE == "Test",]
      d_test
    })
    
    getRec <- reactive({
        rec <- recipes::recipe(DEATH_RATE ~., data = getTrain()) %>%
        update_role("CODE", new_role = "id") %>%
        update_role("OBS_TYPE", new_role = "split") %>%
        step_impute_knn(all_predictors(), neighbors = input$knn) %>%
        step_center(all_numeric(), -has_role("outcome"))%>%
        step_scale(all_numeric(), -has_role("outcome")) %>%
        step_dummy(all_predictors(), -all_numeric())
        rec
      })
    
    getMod <- reactive({
      train(getRec(), data = getTrain(), method = "glmnet")
    })
    
    getPre <- reactive({
      predict(getMod(), newdata = getTest())
    })
    
    output$summary <- renderUI({
      view(dfSummary(d), headings = FALSE, method = 'render', footnote = NA)
    })
    
    output$model <- renderPrint({
      getMod()
    })
    
    output$modelplot <- renderPlot({
      plot(getMod())
    })
    
    output$RMSE <- renderPrint({
      caret::RMSE(getPre(), getTest()$DEATH_RATE, na.rm = FALSE)
    })
    
    output$prediction <- renderPlot({
      rang <- range(c(getTest()$DEATH_RATE, getPre()))
      ggplot(data = getTest()) +
        geom_point(mapping = aes(x = DEATH_RATE, y = getPre())) +
        geom_abline(slope = 1, col = "blue") +
        labs(title = "", y = "predicted", x = "actual") +
        coord_fixed(ratio = 1, xlim = rang, ylim = rang, expand = TRUE)
    })
    
    output$residualplot_test <- renderPlot({
      residuals_test <- getTest()$DEATH_RATE - getPre()
      names(residuals_test) <- rownames(getTest())
      limits <- boxplot.stats(x = residuals_test, coef = input$coef)$stats
      label <- ifelse(residuals_test < limits[1] | residuals_test > limits[5], names(residuals_test), NA)
      d_residuals_test <- data.frame(residuals_test, label)
      ggplot(d_residuals_test, mapping = aes(x = residuals_test, y = 0)) +
        geom_boxplot(coef = input$coef, outlier.colour = "red") +
        ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
        labs(title = paste("Test residual box-plot using", input$coef, "as IQR Multiplier")) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    output$residualplot_train <- renderPlot({
      residuals_train <- getTrain()$DEATH_RATE - getPre()
      names(residuals_train) <- rownames(getTrain())
      limits <- boxplot.stats(x = residuals_train, coef = input$coef)$stats
      label <- ifelse(residuals_train < limits[1] | residuals_train > limits[5], names(residuals_train), NA)
      d_residuals_train <- data.frame(residuals_train, label)
      ggplot(d_residuals_train, mapping = aes(x = residuals_train, y = 0)) +
        geom_boxplot(coef = input$coef, outlier.colour = "red") +
        ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
        labs(title = paste("Train residual box-plot using", input$coef, "as IQR Multiplier")) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    output$residualplot <- renderPlot({
      residuals <- getClean()$DEATH_RATE - getPre()
      names(residuals) <- rownames(getClean())
      limits <- boxplot.stats(x = residuals, coef = input$coef)$stats
      label <- ifelse(residuals < limits[1] | residuals > limits[5], names(residuals), NA)
      d_residuals <- data.frame(residuals, label)
      ggplot(d_residuals, mapping = aes(x = residuals, y = 0)) +
        geom_boxplot(coef = input$coef, outlier.colour = "red") +
        ggrepel::geom_text_repel(max.overlaps = 50, mapping = aes(label = label)) +
        labs(title = paste("Test & Train residual box-plot using", input$coef, "as IQR Multiplier")) +
        theme(axis.title.y = element_blank(), axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    output$predictionplot <- renderPlot({
      hist(getPre(), xlab = "Prediction", main = "test-RMSE statistic", breaks = 20)
    })
    
    output$pairs <- renderPlot({
      ggpairs(data = d[, input$variables_pairs], mapping = aes(color = d[, input$color]), title = "Pairs Plot")
    })
    
    output$correlation <- renderPlot({
      corrgram(d, 
               order = input$order,
               abs = input$absolute, 
               cor.method = input$method)
    })
    
    output$vismiss <- renderPlot({
      vis_miss(d, cluster = input$cluster_missing, sort_miss = input$sort_missing)
    })
    
    output$ggmiss <- renderPlot({
      gg_miss_upset(data = d, nsets = input$nsets) 
    })
    
    output$binarymiss <- renderPlot({
      corrgram(cor(d_shadow), 
               order = input$order_miss,
               abs = input$absolute_miss, 
               cor.method = input$method_miss)
    })
    
    output$predictmiss <- renderPlot({
      tree <- train(MISSINGNESS ~ . -CODE,
                    data = d, method = "rpart", na.action = na.rpart)
      rpart.plot(tree$finalModel, main = "TUNED: Predicting the number of missing variables in an observation",
                 roundint = TRUE, clip.facs = TRUE) 
    })
    
    output$continuity <- renderPlot({
      d_continuity <- d[,input$variables_continuity]
      for (col in 1:ncol(d_continuity)) {
        d_continuity[,col] <- d_continuity[order(d_continuity[,col]),col]
      }
      d_continuity <- scale(x = d_continuity, center = input$center_continuity, scale = input$scale_continuity)
      mypalette <- rainbow(ncol(d_continuity))
      matplot(x = seq(1, 100, length.out = nrow(d_continuity)), y = d_continuity, type = "l", xlab = "Percentile", ylab = "Values", lty = 1, lwd = 1, col = mypalette)
      legend(legend = colnames(d_continuity), x = "topleft", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d_continuity)^0.3))
    })
    
    output$distribution <- renderPlot({
      ggplot(data = d, mapping = aes(x = d[, input$variable_histogram])) +
        geom_histogram(bins = input$bins) +
        labs(x = input$variable_histogram)
    })
    
    output$homogeneity <- renderPlot({
      d_homogeneity <- scale(d[,input$variables_homogeneity], center = input$center_homogeneity, scale = input$scale_homogeneity) 
      mypalette <- rainbow(ncol(d_homogeneity))
      matplot(d_homogeneity, type = "l", col = rainbow(ncol(d_homogeneity)), xlab = "Observations in sequence", ylab = "Value") 
      legend(legend = colnames(d_homogeneity), x = "topright", y = "top", lty = 1, lwd = 1, col = mypalette, ncol = round(ncol(d)^0.3))
    })
    
    output$boxplot <- renderPlot({
      d_boxplot <- scale(d[, input$variables_boxplot], center = input$center_boxplot, scale = input$scale_boxplot) 
      Boxplot(d_boxplot, range = input$IQR)
    })
    
    output$mosaic <- renderPlot({
      formula <- as.formula(paste("~", paste(input$variables_mosaic, collapse = " + ")))
      mosaic(formula, data = d,
                  main = "Frequency Novelties", shade = TRUE, legend = TRUE)
    })
    
    output$raw <- DT::renderDataTable({
      DT::datatable(as.data.frame(d))
    })
    
    
    
})
