rm(list=ls())
if(!require(shiny)) install.packages("shiny")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(DT)) install.packages("DT")
if(!require(DT)) install.packages("shinydashboard")
if(!require(DT)) install.packages("Rcpp")
library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(Rcpp)


dashboardPage(
  dashboardHeader(
    title = "Estadística computacional",
    #    title = tags$a(href='http://mycompanyishere.com',
    #                   tags$img(src='logo_itam_70.png')),
    titleWidth = "900px"
    ),
  skin = "green",
  dashboardSidebar(
    sidebarMenu(
      menuItem("Generación de Números Aleatorios", tabName = "tarea1", icon = icon("black-tie")),
      menuItem("Integracion Numèrica con Monte Carlo", tabName = "tarea2", icon = icon("magic")),
      menuItem("MCMC (Set-up)", tabName = "tarea4", icon = icon("line-chart")),
      menuItem("Markov Chain Monte Carlo", tabName = "tarea5", icon = icon("line-chart")),
      menuItem("Extras", tabName = "extras", icon = icon("gift"))
      
      #menuItem("About", tabName = "about", icon = icon("user"))
    )
  ),
  dashboardBody(
    #tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")),
    ## slide
    tabItems(
      tabItem(tabName = "tarea1",
              h2("Tarea 01: Generación de Números Aleatorios"),
              tabsetPanel(
                tabPanel("Instrucciones",
                         box(
                           width = 15,
                           includeMarkdown("md/hw01.md")
                         )
                ),
                tabPanel("Teoría",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/teo01.md")
                         )
                ),
                tabPanel( "Ejemplo",
                          fluidRow(
                            box(title = "Parámetros", "", width=12,
                                fluidRow(column(8, offset = 1, sliderInput("num", h5("Cantidad de números a simular"),
                                                                           min = 20, max = 1000,
                                                                           value = 50)),
                                         column(4, offset= 1, sliderInput("bins", h5("Cantidad de barras en histograma"),
                                                                          min = 5, max = 100, value = 10)),
                                         column(4, offset= 1, sliderInput("lda", h5("Parametro Lamda para fun(exp)"),
                                                                          min = 1, max = 50, value = 1))
                                )
                                
                            ) 
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
                                                      "Geometrica"             = "GEOM")))
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
              ),
      ##########################################################
      #########---Integracion MonteCarlo----###################
      tabItem(tabName = "tarea2",
              h2("Tarea 02: Integración Numerica con MonteCarlo"),
              tabsetPanel(
                tabPanel("Instrucciones",
                           box(
                             width = 15,
                             includeMarkdown("md/hw02.md")
                           )
                  ),
                  tabPanel("Teoría",
                           box(
                             withMathJax(),
                             width = 15,
                             includeMarkdown("md/teo02.md")
                           )
                  ),
                  tabPanel( "Ejemplo",
                            fluidRow(
                              sidebarLayout(
                                sidebarPanel(
                                  textInput(inputId="inpFunc", label="Funcion a evaluar",
                                            value="function (x) {4/(1+x^2)}"),
                                  
                                  sliderInput(inputId = "lmts", label="Limites de la inegral",
                                              max=10, min=0, value=c(0,1)),
                                  
                                  sliderInput(inputId = "alfa", label="Intervalo de confianza",
                                              max=0.1, min=0.01, value=0.05, step=0.01),
                                  
                                  sliderInput("n", 
                                              "Number of random points:", 
                                              value = 100,
                                              min = 2, 
                                              max = 1000)
                                ),
                                
                                mainPanel(
                                  tabsetPanel(type = "tabs", 
                                              tabPanel("Simulacion MonteCarlo", plotOutput("plot")), 
                                              tabPanel("Intervalos", plotOutput("intervals")),
                                              tabPanel("Trapecio vs MonteCarlo", dataTableOutput("comparation"))
                                  )
                                )
                              )
                          )
                  )
                )
      ),## fin slide menu
      
      ##########################################################
      #########---Set Up Inicial----###################
      tabItem(tabName = "tarea4",
              h2("Tarea 04"),
              tabsetPanel(
                tabPanel("Generalidades",
                         box(
                           width = 15,
                           includeMarkdown("md/hw4-6.md")
                         )
                ),
                
                tabPanel("Teoria",
                         box(
                           withMathJax(),
                           width = 15,
                           "falta"
                           #includeMarkdown("md/teo02.md")
                         )
                ),
                tabPanel("Set up",
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput("cVariables", h3("Variables"),
                                                choices = names(data)),
                             h4("Parámetros aPriori"),
                             sliderInput("t4_s_a", "a -> Unif ", min=1, max=10, value=c(5,8)),
                             sliderInput("t4_s_b", "b <- Norm", min=1, max=10, value=5),
                             sliderInput("t4_s_sigma", "sigma -> Unif", min=1, max=10, value=c(5, 6))
                           ),
                           
                           mainPanel(
                             tabsetPanel(type = "tabs", 
                                         tabPanel("datos",
                                                  fluidRow(
                                                    column(8, plotOutput("plot_data")),
                                                    column(12, DT::dataTableOutput("table"))
                                                  ) 
                                         ),
                                         tabPanel("distribuciones aPriori",
                                                  fluidRow(
                                                    column(4, plotOutput("plot_hist_A")),
                                                    column(4, plotOutput("plot_hist_B")),
                                                    column(4, plotOutput("plot_hist_Sd")),
                                                    column(4, plotOutput("plot_hist_Total"))
                                                  )
                                         )
                             )
                             
                           )
                         )
                ),
                tabPanel("Tarea 05",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw05.md")
                         )
                ),
                tabPanel("Tarea 06",
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/hw06.md")
                         )
                )
              )
      ),## fin slide menu
      
      tabItem(tabName = "extras",
              h2("Extras"), 
              tabsetPanel(
                tabPanel( "Método de la Función Inversa", icon = icon("asterisk"),
                fluidRow(
                  
               
               )),
               tabPanel( "Paradoja del cumpleaños",icon = icon("birthday-cake"),
                         h3("¿Cuál es la probabilidad de que dos personas en una fiesta cumplan años el mismo dia?"),
                         fluidRow(
                           # box(
                           # )
                         ),
                         
                         box(
                           withMathJax(),
                           width = 15,
                           includeMarkdown("md/paradox02.md")
                         ))
               
        ))
    )
  )
)