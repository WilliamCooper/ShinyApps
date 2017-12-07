#
# This is a Shiny web application showing airflow for a gust probe. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(png)
ima <- readPNG('images/GustProbe.png')
dplast <- 0
aoalast <- 0
dPlast <- 1000
aalast <- 1000
p1 <- 700
T1 <- 0
v1 <- 150
dpr <- 0.5 * p1 * v1^2 / (287.05 * (T1 + 273.15))


ui <- fluidPage(
   
   # Application title
   titlePanel("Gust Probe"),
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('Five-Port Sensor',
     sidebarLayout(
       sidebarPanel(
         includeHTML('HTML/GustPodA.html')
       ),
       mainPanel(
         imageOutput('photo')
       )
     )),
    tabPanel('Demonstration of Response',

   sidebarLayout(
      sidebarPanel(
        sliderInput ('pressure', 'pressure [hPa]:', value=p1, min=150, max=1000),
        sliderInput ('temperature', 'temperature [deg C', value=T1, min=-80, max=40),
        sliderInput ('airspeed', 'airspeed [m/s]', value=v1, min=10, max=300),
        sliderInput ('aoa', 'angle of attack [degrees]', value=0, min=-30, max=30, step=0.01),
        sliderInput ('deltapa', 'pressure difference a', value=0, min=-200, max=200, step=0.01),
        actionButton ('endc', 'stop cycling if necessary'),
        textOutput ('dpr')),
      mainPanel(
         plotOutput("flowPlot"),
        includeHTML('HTML/GustPodB.html')
      )
   )
   ))
)

server <- function(input, output, session) {
  
  reac <- reactiveValues(updateaoa=0, updatedp=0)
   
  output$photo <- renderImage({
    list(src = 'images/GustProbe858.jpg',
      contentType = 'image/jpg',
      width = 800,
      height = 600,
      alt = "858 probe image")
  }, delete=FALSE)
  
  observeEvent (input$endc, {
    isolate(reac$updatedp <- dplast)
    isolate(reac$updateaoa <- aoalast)
  })
  
   output$flowPlot <- renderPlot({
     update1 <- (reac$updatedp != dplast)
     update2 <- (reac$updateaoa != aoalast)
     p <- isolate(input$pressure)
     T <- isolate(input$temperature) + 273.15
     v <- isolate(input$airspeed)
     aoa <- isolate(input$aoa) * pi / 180
     dP <- isolate(input$deltapa)
     offset <- 45 * pi / 180
     cso <- cos(offset) * sin(offset)
     deltaP <- 0.5 * p / (287.05 * T) * v^2 * (- (9/4)*(sin(aoa+offset)^2 - sin(aoa-offset)^2))
     aa <- -0.5 * asin(dP / (cso*9*p*v^2/(4*287.05*T)))*180/pi
     print (sprintf ('deltaP is %.3f, aa is %.3f; dP=%.3f, aoa=%.3f', deltaP, aa, dP, aoa*180/pi))
     print (sprintf ('update1=%s, update2=%s', update1, update2))
     if (update1) {updateSliderInput (session, 'deltapa', value=deltaP)}
     if (update2) {updateSliderInput (session, 'aoa', value=aa)}
     dPlast <<- deltaP
     aalast <<- aa
     plot(c(-10,10), c(-10,10), type='n', axes=FALSE, xlab='', ylab='')
     rasterImage(ima, -10, -40, 0, 10)
     da <- -2.95*sin(aoa)*3
     da2 <- da * 0.4 / 2.95 
     for (i in (-2:2)*2) {
       lines(c(-2.95,0), c(1.05+i, 1.05+i+da), col='blue', lty=2, lwd=2)
       lines(c(-2.55,-2.95, -2.55), c(1.05+i-.4+da2, 1.05+i, 1.05+i+.4+da2), col='blue')
     }
     text (-1, 4.3, 'airflow direction', adj=0, srt=-aoa*180/pi*0.6, cex=1.5)
     aoalast <<- reac$updateaoa
     if (update1 && !update2) {aoalast <<- reac$updateaoa + 1}
     dplast <<- reac$updatedp
     if (update2 && !update2) {dplast <<- reac$updatedp + 1}
   })
   
   observeEvent(input$aoa, {
     isolate(reac$updatedp <- reac$updatedp + 1)
   }) 
   
   observeEvent(input$deltapa, {
     isolate(reac$updateaoa <- reac$updateaoa + 1)
   }) 
   
   observeEvent(input$pressure, {
     isolate(reac$updatedp <- reac$updatedp + 1)
   })   
   observeEvent(input$temperature, {
     isolate(reac$updatedp <- reac$updatedp + 1)
   })   
   observeEvent(input$airspeed, {
     isolate(reac$updatedp <- reac$updatedp + 1)
   })
   
   output$dpr <- renderText ({
     p <- input$pressure
     v <- input$airspeed
     T <- input$temperature + 273.15
     aoa <- input$aoa * pi / 180
     dpr <- 0.5 * p * v^2 / (287.05 * T)* (1 - (2.25 * sin(aoa)^2))
     e <- sprintf ('dynamic pressure at leading input port is %.3f hPa', dpr)
     e
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

