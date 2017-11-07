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



ui <- fluidPage(
   
   # Application title
   # titlePanel("differential pressure sensor"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("pressure",
                     "pressure [hPa]",
                     min = 900,
                     max = 1100,
                     value = 1000),
        sliderInput('temperature', 'temperature [deg C]',
                     min=0, max=40, value=20),
        textOutput ('terror'),
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

server <- function(input, output) {
  
  output$terror <- renderText ({
    p <- input$pressure
    t <- input$temperature + 273.15
    pr <- 1000 * (t / 293.15)
    err <- 1000 - pr
    sprintf ('pressure error caused by temperature: %.1f hPa', err)
  })
   
   output$sensorDiagram <- renderPlot({
     p <- input$pressure 
     pm <- p
     pref <- 1000 * (input$temperature + 273.15) / 293.15
     p <- p + (1000 - pref)
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
     exr <- 4
     if (p > 1000) {
       r <- 0.6 + (1100 - p)^exr / 100^exr * 5
       ycircle <- 8.82 + sqrt(r^2 - ((1.8-0.68)/2)^2)
       ypCircle <- ycircle - sqrt(r^2 - (xpCircle-xcircle)^2)
     } else {
       r <- 0.6 + abs(900 - p)^exr / 100^exr * 5
       ycircle <- 8.82 - sqrt(r^2 - ((1.8-0.68)/2)^2)
       ypCircle <- ycircle + sqrt(r^2 - (xpCircle-xcircle)^2)
     }
     lines(xpCircle, ypCircle, col='blue', lwd=2)
     text(xcircle, 12.2, labels=sprintf('measurand: p=%.0f hPa', pm))
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

