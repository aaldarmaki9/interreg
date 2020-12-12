#' @title Interactive Stepwise Regression Model
#'
#' @description
#'
#' \code{stepreg}builds an interactive stepwise regression application for the inputted data set.
#'
#' @details
#' This function builds an interactive stepwise regression application for the inputted data set and includes model summary, distribution of variables,
#' linear assumptions plots, fitted and residuals graphs and table values, and correlation matrix of the variables.
#'
#' @param data an object of class data frame.
#'
#' @import tidyr corrplot ggplot2 dplyr relaimpo shiny MASS
#' @export
#' @return an interactive shiny application for choosing outcome variable from the data set inputted to run stepwise regression.
#' @examples
#' \dontrun{stepreg(Boston)}
#'
stepreg <- function(data){
  if(!require(dplyr)){
    install.packages("dplyr")
    library(dplyr)
  }
  if(!require(shiny)){
    install.packages("shiny")
    library(shiny)
  }
  if(!require(MASS)){
    install.packages("MASS")
    library(MASS)}
  if(!require(ggplot2)){
    install.packages("ggplot2")
    library(ggplot2)
  }
  if(!require(relaimpo)){
    install.packages("relaimpo")
    library(relaimpo)
  }
  if(!require(tidyr)){
    install.packages("tidyr")
    library(tidyr)
  }
  if(!require(corrplot)){
    install.packages("corrplot")
    library(corrplot)
  }

  ui <- fluidPage(
    titlePanel("Stepwise Regression Model" ),
    sidebarLayout(
      sidebarPanel(
        selectInput("outcome", label = h3("Outcome"),
                    choices = colnames(data), selected = 1),
        selectInput("method", label = h3("Stepwise Method"),
                    choices = c("both", "forward", "backward"), selected = 1),
        selectInput("examine", label = h3("Choose an Explanatory Variable to Examine"),
                    choices = colnames(data), selected = 1)

      ),

      mainPanel(

        tabsetPanel(type = "tabs",

                    tabPanel("Scatterplot", plotOutput("scatterplot", height = "700px")), # Plot
                    tabPanel("Distribution", # Plots of data distributions
                             fluidRow(
                               column(6, plotOutput("distribution1", height = "700px", width = "700px")),
                               column(6, plotOutput("distribution2", height = "300px", width = "300px")))
                    ),
                    tabPanel("Stepwise Model and Summary Statistics", verbatimTextOutput("summary"), verbatimTextOutput(outputId = 'summarystatistic'), verbatimTextOutput(outputId = 'mml')),
                    tabPanel("Relative Importance", plotOutput("relative")),
                    tabPanel("Linear Regression Assumptions", plotOutput("assump")),
                    tabPanel("Fitted and residuals graph", plotOutput("Fittedgraph")),
                    tabPanel("Fitted and residuals values", DT::dataTableOutput("value")),
                    tabPanel("Data", DT::dataTableOutput('tbl')), # Data as datatable
                    tabPanel("Corrmatrix", plotOutput("corrmatrix"), height="700px")
                    )

        )
      )
    )


  # SERVER
  server <- function(input, output) {
    # Stepwise Regression output
    output$summary <- renderPrint({
      fit <- lm(reformulate(colnames(data), input$outcome), data=data)
      step.model <- stepAIC(fit, direction = input$method,
                            trace = FALSE)
      summary(step.model)
    })

    # Data Stat Summary
    output$summarystatistic <- renderPrint({
      psych::describe(data)
    })

    # Test linear model assumptions
    output$assump <- renderPlot({
      fit <- lm(reformulate(colnames(data), input$outcome), data=data)
      step.model <- stepAIC(fit, direction = input$method,
                            trace = FALSE)
      par(mfrow = c(2, 2))
      plot(step.model)

    })
    # Relative importance
    output$relative <- renderPlot({
      if(input$method!="forward"){
        fit <- lm(reformulate(colnames(data), input$outcome), data=data)
        step.model <- stepAIC(fit, direction = input$method,
                              trace = FALSE)
        plot(calc.relimp(step.model))
      }

    })

    #Fitted values and residuals graph
    output$Fittedgraph <- renderPlot({
      fit <- lm(reformulate(colnames(data), input$outcome), data=data)
      step.model <- stepAIC(fit, direction = input$method,
                            trace = FALSE)
      data$predicted <- predict(step.model)
      data$residuals <- residuals(step.model)
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
      fit <- lm(reformulate(colnames(data), input$outcome), data=data)
      step.model <- stepAIC(fit, direction = input$method,
                            trace = FALSE)
      data$predicted <- predict(step.model)
      data$residuals <- residuals(step.model)
      t <-data %>% select(input$outcome, predicted, residuals)
      DT::datatable(t, options = list(lengthChange = FALSE))
    })

    # Data output
    output$tbl = DT::renderDataTable({
      DT::datatable(data, options = list(lengthChange = FALSE))
    })


    # Scatterplot output
    output$scatterplot <- renderPlot({
      plot(data[,input$examine], data[,input$outcome], main="Scatterplot",
           xlab=input$examine, ylab=input$outcome, pch=19)})

    # Histogram output
    output$distribution1 <- renderPlot({
      t <-function(data, fill="red", bins=20){
        index <- sapply(data, is.numeric)
        qdata <- data[index]
        qdata_long <- tidyr::gather(qdata)
        ggplot(data=qdata_long, aes(x=value)) +
          geom_histogram(fill=fill, bins=bins) +
          facet_wrap(~key, scale="free") +
          theme_bw()}
        t(data)})


    # Correlation Matrix
     output$corrmatrix <- renderPlot({
        M<-cor(data)
         corrplot(M, method="color")
        }

        )

  }

  shinyApp(ui = ui, server = server)

}






