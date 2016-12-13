if(!require(shiny)) install.packages("shiny")
set.seed(24082016)
library(shiny)
#library(ggplot2)

LCG <- function(nsim, M = 2^32, a = 22695477, c = 1, seed = 110104){
  X = c(seed, numeric(nsim-1)) # Aparta memoria
  for(i in 1:(nsim-1)) X[i+1] <- ((a*X[i] + c)%% M) # Aplica GenradorCongruenciaLineal
  return(X/M) # Aplica transformacion
}

ui <- fluidPage(
  h1("Tarea1 Generador de números aleatorios"),
  h6("Angel Farid Fajardo Oroz"),
  h6("MCC ITAM"),
  
  fluidRow(column(4, offset = 4, sliderInput("num", h5("Cantidad de números a simular"),
              min = 20, max = 1000,
              value = 50))
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

server <- function(input, output) {
  data <- reactive({
    switch(input$radioBtn,
           #generador de congruencia lineal, distribución uniforme
    UNIF = {
      LCG(input$num)
      # sapply(aux[2:input$num], function(x, a=22695477, c=1, m=2**32){
      #   return(((a*x + c) %% m) / m)
      #   })
    },
    
   #Box-Müller
    NORM = { #rnorm(input$num)
      u1 <- runif(input$num)   #R2 <- -2*log(u1)
      u2 <- runif(input$num)   #theta <- 2*pi*u2
      sqrt(-2*log(u1))*cos(2*pi*u2)
      },
    #función inversa
    EXP  = {
      sapply(seq(1, input$num), function(x, lambda=10){
        u <- runif(length(x))
        return(-log(1-u)/lambda)
      })
    },
   
    GEOM = {
      sapply(seq(1, input$num), function(x, prob=0.5){
        u <- runif(length(x))
        return(log(u)/log(1-prob))
      })
    })
  })
  
  kolmogorovTest <- reactive({
    switch(input$radioBtn,
           UNIF = ks.test(data(), "punif"),
           NORM = ks.test(data(), "pnorm"),
           EXP  = ks.test(data(), "pexp"),
           GEOM = ks.test(data(), rgeom(input$num, prob=0.5)))
  })
  
  chiTest <- reactive({
    switch(input$radioBtn,
           UNIF = {breaks <- seq(0,1, length.out = input$num/10)
                   o <- table(cut(data(), breaks=breaks))
                   p <- diff(punif(breaks))
                   chisq.test(o, p=p, rescale.p=T)},
           NORM = {breaks <- c(seq(-5,5, length.out = input$num/10))
                   o <- table(cut(data(), breaks = breaks))
                   p <- diff(pnorm(breaks))
                   chisq.test(o, p=p, rescale.p = T)},
           EXP  = {breaks <- c(seq(0,10, length.out = input$num/10))
                   o <- table(cut(data(), breaks = breaks))
                   p <- diff(pexp(breaks))
                   chisq.test(o, p=p, rescale.p = T)},
           GEOM = {breaks <- c(seq(0,10, by=1))
                   o <- table(cut(data(), breaks = breaks))
                   p <- diff(pgeom(breaks, prob=0.5))
                   chisq.test(o, p=p, rescale.p = T)}
           )
  })
  
  output$ksTest <- renderPrint({
    kolmogorovTest()
  })
  
  output$chiTest <- renderPrint({
    chiTest()
  })
  
  output$stats <- renderPrint({
    summary(data())
  })
  
  output$hist <- renderPlot({
    h <- hist(data(), breaks = 20, plot=F)
    d <- density(data())
    hist(data(), breaks = 20,
         main= isolate(input$title))
    lines(x=d$x, y=d$y*length(data())*diff(h$breaks)[1], ldw=2)
  })
  
  output$qqPlot <- renderPlot({
    par(mfrow=c(1,2))
    switch(input$radioBtn,
           UNIF = {q1 = qunif(seq(0,1,0.01)); 
                   q2 = quantile(data(), seq(0,1,0.01));
                   plot(q1, q2, main="Q-Q Plot", ylab = "CLG", xlab="qunif")},
           NORM = {q1 = qnorm(seq(0,1,0.01)); 
                   q2 = quantile(data(), seq(0,1,0.01));
                   plot(q1, q2, main="Q-Q Plot", ylab="rnorm", xlab="qnorm")},
           EXP  = {q1 = qexp(seq(0,1,0.01));  
                   q2 = quantile(data(), seq(0,1,0.01));
                   plot(q1, q2, main="Q-Q Plot", ylab = "expInv", xlab="qexp")},
           GEOM = {q1 = qgeom(seq(0,1,0.01), prob=0.5);  
                   q2 = quantile(data(), seq(0,1,0.01));
                   plot(q1, q2, main="Q-Q Plot", ylab = "geomInv", xlab="qgeom")}
      )
    plot(data()[1:length(data())-1], data()[2:length(data())], main = "Secuencia en números")
    #qplot(data()[-length(data())], data()[-1], main = "Secuencia en números")
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste(input$radioBtn, '.csv', sep='') },
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
}

shinyApp(ui = ui, server = server)

#shiny::runGitHub("compstat2016", "farid7", subdir = "Tarea1_GeneradorNumerosAleatorios")
#shiny::runGitHub("compstat2016", "carlosurteaga")
