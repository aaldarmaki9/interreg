#' @title Interactive Simple Linear Regression
#'
#' @description
#' \code{slreg} builds an interactive simple linear regression application for the inputted dataset.
#' @details
#' This function builds a user-friendly simple linear regression shiny application for the inputted dataset and includes model summary, data summary,
#'distribution of variables, linear assumptions plots, fitted and residuals graphs and table values, and correlation matrix of the variables.
#'
#' @param data an object of class dataframe.
#' @export
#' @import shiny broom ggplot2 corrplot
#'
#' @return an interactive shiny application for choosing outcome and independent variables from the dataset inputted to run simple linear regression on.
#' @examples
#' # \dontrun{slreg(swiss)}

slreg <- function(data){
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  if(!require(broom)){
    install.packages("broom")
    library(broom)}
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(corrplot)){
    install.packages("corrplot")
    library(corrplot)}
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)}
  ui <- fluidPage(
    titlePanel(paste("Linear Regression Model (Dataset:", deparse(substitute(data)),")")),
    sidebarLayout(
      sidebarPanel(
        selectInput("outcome", label = h3("Outcome"),
                    choices = colnames(data), selected = 1),

        selectInput("indepvar", label = h3("Explanatory variable"),
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
                    tabPanel("Fitted values and residuals graph", plotOutput("Fittedgraph")),
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
      fit <- lm(data[,input$outcome] ~ data[,input$indepvar])
      names(fit$coefficients) <- c("Intercept", input$indepvar)
      summary(fit)
    })

    # Data Stat Summary
    output$summarystatistic <- renderPrint({
      psych::describe(data)
    })

    # Data output
    output$tbl = DT::renderDataTable({
      DT::datatable(data, options = list(lengthChange = FALSE))
    })

    #Fitted values and residuals
    output$Fittedgraph <- renderPlot({
      fit <- lm(data[,input$outcome] ~ data[,input$indepvar])
      model.diag.metrics <- augment(fit)
      ggplot(model.diag.metrics, aes(data[,input$indepvar], data[,input$outcome])) +
        xlab(input$indepvar)+
        ylab(input$outcome)+
        geom_point() +
        stat_smooth(method = lm, se = FALSE) +
        geom_segment(aes(xend = data[,input$indepvar], yend = .fitted), color = "red", size = 0.3)

    }

    )
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
      ggplot(data, aes(x=data[,input$indepvar], y=data[,input$outcome])) +
          geom_point()+
          geom_smooth(method=lm)+ xlab(input$indepvar)+ylab(input$outcome)
    })


    # Histogram output var 1
    output$distribution1 <- renderPlot({
      hist(data[,input$outcome], main="", xlab=input$outcome)
    })

    # Histogram output var 2
    output$distribution2 <- renderPlot({
      hist(data[,input$indepvar], main="", xlab=input$indepvar)
    })

    # Correlation Matrix
    output$corrmatrix <- renderPlot({
      M<-cor(data)
      corrplot(M, method="color")
    })

    # Test linear model assumptions
    output$assump <- renderPlot({
      par(mfrow = c(2, 2))
      plot(lm(data[,input$outcome] ~ data[,input$indepvar]))

    })






  }

  shinyApp(ui = ui, server = server)



}









