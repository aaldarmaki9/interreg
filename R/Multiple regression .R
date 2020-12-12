#' Interactive Multiple Linear Regression
#'
#'@description
#' \code{multreg} builds an interactive multiple linear regression application for the inputted dataset.
#'  @details
#'  This function builds an interactive multiple linear regression application for the inputted dataset and includes model summary, distribution of variables,
#' linear assumptions plots, fitted and residuals graphs and table values, and correlation matrix of the variables.
#' @param data an object of class dataframe.
#' @export
#' @import shiny dplyr ggplot2 corrplot tidyr
#'
#' @return an interactive shiny application for choosing outcome and independent variables from the dataset inputted to run multiple linear regression on.
#' @examples
#' #\dontrun{multreg(mtcars)}

multreg <- function(data){
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(corrplot)){
    install.packages("corrplot")
    library(corrplot)}
  if(!require(tidyr)){
    install.packages("tidyr")
    library(tidyr)
  }
  ui <- fluidPage(
    titlePanel(paste("Multiple Regression Model (Dataset:", deparse(substitute(data)),")")),
    sidebarLayout(
      sidebarPanel(
        selectInput("outcome", label = h3("Outcome"),
                    choices = colnames(data), selected = 1),

        selectInput("indepvar", label = h3("Explanatory variable"),
                    choices = colnames(data), multiple = TRUE, selected = Inf),

        selectInput("examine", label = h3("Choose an Explanatory Variable to Examine"),
                    choices = colnames(data), selected = 1)

      ),

      mainPanel(

        tabsetPanel(type = "tabs",

                    tabPanel("Scatterplot", plotOutput("scatterplot", height = "700px")), # Plot
                    tabPanel("Distribution", # Plots of data distributions
                             fluidRow(
                               column(6, plotOutput("distribution1", height = "300px", width = "300px")),
                               column(6, plotOutput("distribution2", height = "300px", width = "300px")))
                    ),
                    tabPanel("Linear Model and Summary Statistics", verbatimTextOutput("summary"), verbatimTextOutput(outputId = 'summarystatistic')),# Regression output
                    tabPanel("Linear Regression Assumptions", plotOutput("assump")),
                    tabPanel("Fitted and residuals graph", plotOutput("Fittedgraph")),
                    tabPanel("Fitted and residuals values", DT::dataTableOutput("value")),
                    tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                    tabPanel("Corrmatrix", plotOutput("corrmatrix"), height="700px")

        )
      )
    ))


  # SERVER
  server <- function(input, output) {
    # Linear Regression output
    output$summary <- renderPrint({
      lm1 <-lm(reformulate(input$indepvar, input$outcome), data = data)
      summary(lm1)
    })

    # Data Stat Summary
    output$summarystatistic <- renderPrint({
      psych::describe(data)
    })

    # Data output
    output$tbl = DT::renderDataTable({
      DT::datatable(data, options = list(lengthChange = FALSE))
    })

    #Fitted values and residuals graph
    output$Fittedgraph <- renderPlot({
      fit <- lm(reformulate(input$indepvar, input$outcome), data = data)
      data$predicted <- predict(fit)
      data$residuals <- residuals(fit)
      ggplot(data, aes(x = data[,input$examine], y = data[,input$outcome])) + xlab(input$examine)+
        ylab(input$outcome)+
        geom_segment(aes(xend = data[,input$examine], yend = predicted), alpha = .2) +  # Lines to connect points
        geom_point(aes(color = residuals)) +
        scale_color_gradient2(low = "blue", mid = "white", high = "red") +
        guides(color = FALSE) +  # Points of actual values
        geom_point(aes(y = predicted), shape = 1) +  # Points of predicted values
        theme_bw()


    })
    #Fitted and residuals values
    output$value <- DT::renderDataTable({
      fit <- lm(reformulate(input$indepvar, input$outcome), data = data)
      data$predicted <- predict(fit)
      data$residuals <- residuals(fit)
      t <-data %>% select(input$outcome, predicted, residuals)
      DT::datatable(t, options = list(lengthChange = FALSE))
    })
    # Scatterplot output
    output$scatterplot <- renderPlot({
      ggplot(data, aes(x=data[,input$examine], y=data[,input$outcome])) +
        geom_point()+
        geom_smooth(method=lm)+ xlab(input$examine)+ylab(input$outcome)
    })


    # Histogram output var 1
    output$distribution1 <- renderPlot({
      hist(data[,input$outcome], main="", xlab=input$outcome)
    })

    # Histogram output var 2
    output$distribution2 <- renderPlot({
      hist(data[,input$examine], main="", xlab=input$examine)
    })

    # Correlation Matrix
    output$corrmatrix <- renderPlot({
      cor(data[,c(input$indepvar)])
      M<-cor(data)
      corrplot(M, method="color")
    })

    # Test linear model assumptions
    output$assump <- renderPlot({
      par(mfrow = c(2, 2))
      plot(lm(reformulate(input$indepvar, input$outcome), data = data))

    })






  }

  shinyApp(ui = ui, server = server)



}





