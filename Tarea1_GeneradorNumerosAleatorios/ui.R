if(!require(shiny)) install.packages("shiny")
library(shiny)
set.seed(24082016)

shinyUI(
  fluidPage(
  h1("Tarea1 Generador de números aleatorios"),
  h6("Angel Farid Fajardo Oroz"),
  h6("MCC ITAM"),
  
  fluidRow(column(4, offset = 4, sliderInput("num", h5("Cantidad de números a simular"),
              min = 20, max = 1000,
              value = 50)),
           column(4, offset= 1, sliderInput("bins", h5("Cantidad de barras en histograma"),
                                            min = 5, max = 100, value = 10)),
           column(4, offset= 1, sliderInput("lda", h5("Parametro Lamda para fun(exp)"),
                                            min = 1, max = 50, value = 1))
          ),
  
  h3("Pruebas de bondad de ajuste"),
  #h5("Kolmogorov-Smirnov y Chi-Square Test"),
  fluidRow(column(5, verbatimTextOutput("ksTest")),
           column(5, verbatimTextOutput("chiTest"))
          ),
  
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(radioButtons("radioBtn", "Tipo de distribución:",
                   c("Uniforme (GCL)"         = "UNIF",
                     "Exponencial (Fnc-Inv)"  = "EXP",
                     "Normal (Box-Müller)"    = "NORM",
                     "Geometrica"             = "GEOM")),
      column(5, offset=1, downloadButton('downloadData', 'Descargar Datos')))
    ),
    
  mainPanel(
    tabsetPanel(
      tabPanel("histograma", plotOutput("hist")),
      tabPanel("qqPlot",     plotOutput("qqPlot"))
      ),
    verbatimTextOutput("stats")
    )
  )
)
)

#shiny::runGitHub("compstat2016", "farid7", subdir = "Tarea1_GeneradorNumerosAleatorios")
