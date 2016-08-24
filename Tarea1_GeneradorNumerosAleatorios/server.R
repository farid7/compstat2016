if(!require(shiny)) install.packages("shiny")
library(shiny)
set.seed(24082016)

shinyServer(function(input, output) {
  data <- reactive({
    switch(input$radioBtn,
           #generador de congruencia lineal
           UNIF = {
             sapply(seq(1,input$num), function(x, a=22695477, c=1, m=2**32){
               return(((a*x + c) %% m) / m)
             })
           },
           #distribución uniforme
           NORM = rnorm(input$num),
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
           UNIF = ks.test(data(), runif(input$num)),
           NORM = ks.test(data(), "pnorm"),
           EXP  = ks.test(data(), "pexp"),
           GEOM = ks.test(data(), rgeom(input$num, prob=0.5)))
  })
  
  chiTest <- reactive({
    switch(input$radioBtn,
           UNIF = {breaks <- c(seq(0,10, by=1))
                   o <- table(cut(data(), breaks = breaks))
                   p <- diff(punif(breaks))
                   chisq.test(o, p=p, rescale.p = T)},
           NORM = {breaks <- c(seq(0,10, by=1))
                   o <- table(cut(data(), breaks = breaks))
                   p <- diff(pnorm(breaks))
                   chisq.test(o, p=p, rescale.p = T)},
           EXP  = {breaks <- c(seq(0,10, by=1))
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
    lines(x=d$x, y=d$y*length(data())*diff(h$breaks)[1])
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
  })
})
