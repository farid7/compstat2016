rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
library(ggplot2)
library(DT)
library(shiny)
#setwd("/Users/LUSI_ITAM/Documents/farid/estadisticaComputacional_Clase/tareas/tarea4_setupReaingTable")
setwd("/home/farid/Documents/estadistica computacional 2016b/tarea4_despligaDatos")
data <- read.csv(file="cheese.csv", header=T)

ui <- fluidPage(
  titlePanel("Basic DataTable"),
  
  sidebarLayout(
    sidebarPanel(
     checkboxGroupInput("cVariables", h3("Variables"),
                              choices = names(data)),
     h4("Parámetros aPriori"),
     sliderInput("a", "a", min=1, max=10, value=5),
     sliderInput("sSigma", "sigma", min=1, max=10, value=5),
     sliderInput("e", "error", min=1, max=10, value=5)
  ),
  
  mainPanel("Tarea 4",
  fluidRow(
    splitLayout(cellWidths=c("50%", "50%"),DT::dataTableOutput("table"), plotOutput("Graph1"))
    ),
  plotOutput("gPriori")
   )
  )
)

server <- function(input, output) {
  n <- dim(data)[1]
  
  dataInput <- reactive({
    if(is.null(input$cVariables))
      return()
    aux <- data[, input$cVariables]
    aux
  })
  
  priori <- reactive({
    rnorm(n, mean=0, sd = input$sSigma)
  })
  
  
  nmes <- renderText({
    input$cVariables
    })
  
   output$table <- DT::renderDataTable(DT::datatable({
     if(is.null(input$cVariables))
       return()
    else 
      return(dataInput())
     
  }))
   
   output$Graph1 <- renderPlot({
     if(is.null(input$cVariables))
       return()
     else{
       # aux1 <- dataInput()[,1]
       # aux2 <- dataInput()[,2]
      return(plot(dataInput()))
     }
   })
 output$gPriori <- renderPlot({
   hist(priori(), title="Priori")
 }) 
}

shinyApp(ui = ui, server = server)
