#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(scales)
library(Ranadu)


ui <- fluidPage(
   
   # Application title
   titlePanel("Vapor Pressure Calculator"),
   
   # Sidebar with entry windows for temperature and total pressure
   sidebarLayout(
      sidebarPanel(
        fluidRow (
          column (6, numericInput ('temp', 'temperature [C]', value=10)),
          column (6, numericInput ('pressure', 'pressure [hPa]', value=1000))),
        checkboxInput('ef', 'include enhancement factor?'),
        htmlOutput ('e'),
        textOutput ('r'),
        textOutput ('q'),
        fluidRow (
          column (6, numericInput ('ewx', 'vapor pressure [hPa]', value=10)),
          column (6, htmlOutput ('dp'))
        ),
        includeHTML('HTML/Calculator.html')
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("ePlot", click=clickOpts(id='plot_click'))
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
   
  output$e <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop (t, p)
      sprintf ('equilibrium vapor pressure is %.3f hPa <br> &emsp; *** (enhancement factor included)', e)
    } else {
      e <- MurphyKoop(t)
    }
    updateNumericInput(session, 'ewx', value=e)
    sprintf ('equilibrium vapor pressure is %.3f hPa', e)
  })
  
  output$r <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p)
    } else {
      e <- MurphyKoop(t)
    }
    r <- MixingRatio (e/p) * 1000
    sprintf ('equilibrium mixing ratio is %.3f g/kg', r)
  })
  
  output$dp <- renderText ({
    e <- input$ewx
    p <- input$pressure
    if (input$ef) {
      dp <- nleqslv::nleqslv(10, function(x) MurphyKoop (x, p)-e)$x
    } else {
      dp <- nleqslv::nleqslv(10, function(x) MurphyKoop (x)-e)$x
    }
    sprintf ('<br><br>dew point is %.3f C ', dp)
  })
  
  output$q <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p)
    } else {
      e <- MurphyKoop(t)
    }
    r <- MixingRatio (e/p)
    q <- r/(1+r) * 1000
    sprintf ('equilibrium specific humidity is %.3f g/kg', q)
  })
  
  observeEvent (input$plot_click, {
    print (input$plot_click)
    ## these coordinates were determined empirically, will change if plot changes
    xcursor <- as.numeric(input$plot_click$x-0.0954)/(0.9765-0.0954)
    t <- -60 + (100 * xcursor)
    updateTextInput (session, 'temp', value=t)
  } )
  
  
   output$ePlot <- renderPlot({
     t <- seq(-60, 40, by=0.1)
     e <- MurphyKoop (t)
     dotT <- input$temp
     dotE <- MurphyKoop (dotT)
     g <- ggplot (data=data.frame (t, e)) +  geom_path (aes(x=t, y=e), colour='blue') +
       geom_point (data=data.frame(dotT, dotE), aes(x=dotT, y=dotE), colour='forestgreen', size=3) +
       xlab('temperature [C]') + ylab('water vapor pressure [hPa]') +
       scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3), #limits = c(xmin, xmax),
       labels = trans_format("log10", math_format(10^.x))) + 
       annotation_logticks(sides='l') + theme_WAC()
     # ggplotly(g)
     print (g)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

