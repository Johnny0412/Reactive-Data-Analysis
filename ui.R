#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
columns <- names(read.csv("Data.csv", header = TRUE))

shinyUI(fluidPage(

    # Application title
    titlePanel("Reactive Data Analysis"),

    tabsetPanel(
        tabPanel("Summary", htmlOutput(outputId = "summary")),
        
        tabPanel("Model Evaluation", 
                 sidebarLayout(
                     sidebarPanel(
                         sliderInput(inputId = "vthreshold", label = "Variable missing rate threshold", min = 0, max = 100, step = 1, value = 20),
                         sliderInput(inputId = "othreshold", label = "Observation missing rate threshold", min = 0, max = 100, step = 1, value = 20),
                         sliderInput(inputId = "knn", label = "KNN neighbours", min = 1, max = 10, step = 1, value = 5),
                         sliderInput(inputId = "coef", label = "Residual IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.2)
                     ),
                     mainPanel(
                         tabsetPanel(
                             tabPanel("Model and Plot", verbatimTextOutput("model"), plotOutput("modelplot")), 
                             tabPanel("Test RMSE", plotOutput("prediction"), verbatimTextOutput("RMSE")),
                             tabPanel("Test Residual Boxplot", plotOutput("residualplot_test")),
                             tabPanel("Train Residual Boxplot", plotOutput("residualplot_train")),
                             tabPanel("Test & Train Residual Boxplot", plotOutput("residualplot"))
                         )))),
        
        tabPanel("Pairs Plot", 
                 column(3, selectInput(inputId = "color", label = "Color", 
                             choices = list("GOVERN_TYPE", "HEALTHCARE_BASIS"), selected= "GOVERN_TYPE")),
                 
                 column(3, selectizeInput(inputId = "variables_pairs", label = "Variables:", 
                                choices = columns, 
                                multiple = TRUE, selected = list("GOVERN_TYPE", "HEALTHCARE_BASIS", "DEATH_RATE"))),
                 
                 plotOutput(outputId = "pairs")),
        
        tabPanel("Correlation Chart", 
                 column(1, checkboxInput(inputId = "absolute", label = "Absolute", value = FALSE)),
                 column(3, selectInput(inputId = "method", label = "Method", choices = list("pearson","spearman","kendall"), selected = "pearson")),
                 column(3, selectInput(inputId = "order", label = "Order", choices = list("OLO","GW","HC"), selected = "OLO")),
                 plotOutput(outputId = "correlation")),
        
        tabPanel("Missing Data", 
                 tabsetPanel(
                     tabPanel("Vismiss",
                              column(1, checkboxInput(inputId = "sort_missing", label = "Sort", value = FALSE)),
                              column(1, checkboxInput(inputId = "cluster_missing", label = "Cluster", value = FALSE)),
                              plotOutput(outputId = "vismiss")),
                     
                     tabPanel("Ggmiss",
                              sliderInput(inputId = "nsets", label = "Number of sets", min = 2, max = 12, step = 1, value = 5),
                              plotOutput(outputId = "ggmiss")),
                 
                     tabPanel("Binary Missingness Correlation",
                              column(1, checkboxInput(inputId = "absolute_miss", label = "Absolute", value = FALSE)),
                              column(3, selectInput(inputId = "method_miss", label = "Method", choices = list("pearson","spearman","kendall"), selected = "pearson")),
                              column(3, selectInput(inputId = "order_miss", label = "Order", choices = list("OLO","GW","HC"), selected = "OLO")),
                              plotOutput(outputId = "binarymiss")),
                 
                     tabPanel("Missingness Prediction",
                              plotOutput(outputId = "predictmiss")))),
        
        tabPanel("Continuity", 
                 column(1, checkboxInput(inputId = "center_continuity", label = "Center", value = FALSE)),
                 column(1, checkboxInput(inputId = "scale_continuity", label = "Scale", value = FALSE)),
                 column(3, selectizeInput(inputId = "variables_continuity", label = "Variables:", 
                                choices = columns[-c(1,2,12,15)], 
                                multiple = TRUE, selected = columns[-c(1,2,12,15)])),
                 plotOutput(outputId = "continuity")),
        
        tabPanel("Distribution", 
                 column(3, selectInput(inputId = "variable_histogram", label = "Variable", 
                             choices = columns[-c(1,2,12,15)])),
                 column(3, sliderInput(inputId = "bins", label = "Number of bins", min = 10, max = 100, step = 10, value = 50)),
                 plotOutput(outputId = "distribution")),
        
        tabPanel("Homogeneity", 
                 column(1, checkboxInput(inputId = "center_homogeneity", label = "Center", value = FALSE)),
                 column(1, checkboxInput(inputId = "scale_homogeneity", label = "Scale", value = FALSE)),
                 column(3, selectizeInput(inputId = "variables_homogeneity", label = "Variables:", 
                                          choices = columns[-c(1,2,12,15)], 
                                          multiple = TRUE, selected = columns[-c(1,2,12,15)])),
                 plotOutput(outputId = "homogeneity")),
        
        tabPanel("Novelties", 
                 tabsetPanel(
                     tabPanel("Boxplot", 
                              column(1, checkboxInput(inputId = "center_boxplot", label = "Center", value = FALSE)),
                              column(1, checkboxInput(inputId = "scale_boxplot", label = "Scale", value = FALSE)),
                              column(3, sliderInput(inputId = "IQR", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5)),
                              column(3, selectizeInput(inputId = "variables_boxplot", label = "Variables:", 
                                                       choices = columns[-c(1,2,12,15)], 
                                                       multiple = TRUE, selected = columns[-c(1,2,12,15)])),
                              plotOutput(outputId = "boxplot")),
                              
                     tabPanel("Mosaic Plot", 
                              selectizeInput(inputId = "variables_mosaic", label = "Variables:", 
                                             choices = columns[c(2,12)], 
                                             multiple = TRUE, selected = columns[c(2,12)]),
                              plotOutput(outputId = "mosaic")))),
        
        tabPanel("Raw Data",
                 dataTableOutput(outputId = "raw"))
        
        
        
        
    
)))
