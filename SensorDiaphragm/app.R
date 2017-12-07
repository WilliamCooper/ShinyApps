#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(png)
ima <- readPNG('images/DiffP.png')
rseq <- c(10, 2, 1, 0.8, 0.6)
ir <- 0
dir <- 1
xcircle <- (1.8+0.68)/2
xpCircle <- seq(0.68, 1.8, by=0.03)
psave <- 1000
asave <- 15
updateDirection <- 1
Cycles <- 3



ui <- fluidPage(
   
   # Application title
   # titlePanel("differential pressure sensor"),
   
   sidebarLayout(
      sidebarPanel(
        radioButtons ('animation', 'animation?', choices=c('none', 'pressure', 'wind angle'), 
                      selected='pressure', inline=TRUE)
      ,  
         sliderInput("pressure",
                     "pressure [hPa]",
                     min = 900,
                     max = 1100,
                     value = 1000),
        sliderInput('temperature', 'temperature [deg C]',
                     min=0, max=40, value=20),
        sliderInput('angle', 'wind angle downward [deg]', min=0, max=30, value=0),
        sliderInput('ws', 'wind speed [m/s]', min=0, max=25, value=25),
        textOutput ('terror'),
        textOutput ('werror'),
        textOutput ('psensed'),
        h4('Information regarding this diagram:'),
        includeHTML('HTML/SensorDiaphragm.html')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
        h2('differential pressure sensor'),
         plotOutput("sensorDiagram"), width=6
      )
   )
)

server <- function(input, output, session) {
  
  reac <- reactiveValues (cycle=0)
  
  exprAnimation <- quote ({
    if (input$animation != 'none') {
      Cycles <<- 5
      isolate(reac$cycle <- reac$cycle + 1)
  #     if (psave == 1100) {
  #       updateDirection <<- -1
  #     }
  #     if (psave == 900) {
  #       updateDirection <<- 1 ycircle <- 8.82 + sqrt(r^2 - ((1.8-0.68)/2)^2)
  #     }
  #     psave <<- psave + updateDirection * 50
  #     updateNumericInput(session, 'pressure', value=psave)
    }
  })
  obsAnimation <- observe (exprAnimation, quoted=TRUE)
  
  output$werror <- renderText ({
    p <- input$pressure
    rho=input$pressure * 100 / (287.05 * (input$temperature+273.15))
    v <- input$ws
    err <- 0.5 * rho * (v*sin(input$angle*pi/180))^2
    sprintf ('pressure error caused by wind: %.1f hPa', err)
  })
  
  output$terror <- renderText ({
    p <- input$pressure
    t <- input$temperature + 273.15
    pr <- 1000 * (t / 293.15)
    err <- 1000 - pr
    sprintf ('pressure error caused by temperature: %.1f hPa', err)
  })  
   
  output$psensed <- renderText ({
    p <- input$pressure 
    pref <- 1000 * (input$temperature + 273.15) / 293.15
    p <- p + (1000 - pref)
    rho=input$pressure * 100 / (287.05 * (input$temperature+273.15))
    v <- input$ws
    err <- 0.5 * rho * (v*sin(input$angle*pi/180))^2
    p <- p + err
    sprintf ('measured pressure: %.1f hPa', p)
  })
   
   output$sensorDiagram <- renderPlot({
     reac$cycle
     p <- input$pressure 
     pm <- p
     pref <- 1000 * (input$temperature + 273.15) / 293.15
     p <- p + (1000 - pref)
     rho=input$pressure * 100 / (287.05 * (input$temperature+273.15))
     v <- input$ws
     err <- 0.5 * rho * (v*sin(input$angle*pi/180))^2
     p <- p + err
     # cycles=5
     # ir <- 0
     # while (cycles) {
     #   Sys.sleep(1)
     #   plot(c(-2,10), c(0,12), type='n', axes=FALSE, xlab='', ylab='')
     #   # if (ir == 1) {dir <- 1}
     #   # if (ir == 5) {
     #   #   dir <- -1
     #   #   cycles <- cycles - 1
     #   # }
     #   # ir <- ir + dir
     #   # rasterImage(ima, -10, -4, 12, 18)
     #   ## draw circle of radius r through the two reference points:
     #   # r <- rseq[ir]
     #   # ycircle <- 8.82 + sqrt(r^2 - ((1.8-0.68)/2)^2)
     #   # ypCircle <- ycircle - sqrt(r^2 - (xpCircle-xcircle)^2)
     #   # lines(xpCircle, ypCircle, col='blue', lwd=2)
     # }
     plot(c(-2,10), c(0,12), type='n', axes=FALSE, xlab='', ylab='')
     rasterImage(ima, -10, -4, 12, 18)
     exr <- 5
     if (p > 1001) {
       r <- 0.6 + ((1300 - p) / 300)^exr * 1.5 #3
       ycircle <- 8.82 + sqrt(r^2 - ((1.8-0.68)/2)^2)
       ypCircle <- ycircle - sqrt(r^2 - (xpCircle-xcircle)^2)
     } else if (p < 999) {
       r <- 0.6 + ((p - 700) / 300)^exr * 1.5 #3
       ycircle <- 8.82 - sqrt(r^2 - ((1.8-0.68)/2)^2) 
       ypCircle <- ycircle - sqrt(r^2 - (xpCircle-xcircle)^2)
       ypCircle <- ycircle + sqrt(r^2 - (xpCircle-xcircle)^2)
     } else {
       r <- 10
       ycircle <- 8.82 + sqrt(r^2 - ((1.8-0.68)/2)^2)
       ypCircle <- ycircle - sqrt(r^2 - (xpCircle-xcircle)^2)
     }
     
     lines(xpCircle, ypCircle, col='blue', lwd=2)
     par(xpd=NA)
     xl <- 2
     xa <- xcircle - xl
     ang <- input$angle
     angr <- ang*pi/180
     yv <- 12.5
     ya <- yv + xl * 2 * sin(angr)
     # da2 <- sin(angr) * 0.2 
     lines (c(xa, xcircle), c(yv, yv), col='black', lty=2)
     lines (c(xa, xcircle), c(ya, yv), col='blue', lwd=2)
     # arrowhead:
     xc=xcircle
     la <- 0.25
     aa <- 45*pi/180
     xa1 <- xc - la*cos(aa+angr)
     xa2 <- xc - la*cos(aa-angr)
     ya1 <- yv + la*sin(aa+angr)
     ya2 <- yv - la*sin(aa-angr)
     # lines(c(xcircle-0.2, xcircle, xcircle-0.2), c(yv-.2+da2, yv, yv+.2+da2), col='blue')
     lines (c(xa1, xc, xa2), c(ya1, yv, ya2), col='blue')
     text(xcircle-0.7, yv+0.6+sin(angr), sprintf ('%.0f deg',ang), srt=-ang*1.2, cex=1.2, col='blue')
     # text(xcircle, 14, labels=sprintf('measurand: p=%.0f hPa', pm))
     title(sprintf ('       measurement: %.0f hPa\n       measurand:     %.0f hPa', p, pm), cex=0.8)
     if (input$animation == 'pressure' && Cycles > 0) {
       if (psave > 1099) {
         updateDirection <<- -1
       }
       if (psave < 901) {
         updateDirection <<- 1
       }
       psave <<- psave + updateDirection * 20
       updateNumericInput(session, 'pressure', value=psave)
       if (abs(psave-1000) < 1 && updateDirection == 1) {
         Cycles <<- Cycles - 1
         if (Cycles == 0) {
           updateCheckboxGroupInput(session, 'animation', selected='none')
         }
       }
       # print (sprintf ('psave=%.1f, Cycles=%d', psave, Cycles))
     } 
     if (input$animation == 'wind angle' && Cycles > 0) {
       if (asave > 29.99) {
         updateDirection <<- -1
       }
       if (asave < 0.01) {
         updateDirection <<- 1
       }
       asave <<- asave + updateDirection * 3
       updateNumericInput(session, 'angle', value=asave)
       if (abs(asave) < .1 && updateDirection == -1) {
         Cycles <<- Cycles - 1
         if (Cycles == 0) {
           updateCheckboxGroupInput(session, 'animation', selected='none')
         }
       }
     } 
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

