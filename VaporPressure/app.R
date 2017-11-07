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
library(xtable)


ui <- fluidPage(
   
   # Application title
   titlePanel("Vapor Pressure Calculator"),
   
   # Sidebar with entry windows for temperature and total pressure
   sidebarLayout(
      sidebarPanel(
        fluidRow (
          column (6, numericInput ('temp', 'temperature [C]', min=-80, max=60, value=10)),
          column (6, numericInput ('pressure', 'pressure [hPa]', value=1000))),
        fluidRow(
          column(6, sliderInput ('rh', 'relative humidity [%]', min=0, max=100, step=1, value=100)),
          column (6, checkboxInput('ef', 'include enhancement factor?'))),
        htmlOutput ('e'),
        textOutput ('r'),
        textOutput ('q'),
        fluidRow (
          column (6, numericInput ('ewx', 'vapor pressure [hPa]', value=10)),
          column (6, htmlOutput ('dp'))
        ),
        fluidRow (
          column (6, textOutput ('MWA')),
          column (6, textOutput ('Ra'))
        ),
        h3('Mole Fractions for Moist Air:'),
        tableOutput('composition'),
        width=3
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("ePlot", click=clickOpts(id='plot_click')),
        includeHTML('HTML/Calculator.html'),
        width=6
      )
   )
)

server <- function(input, output, session) {
  
  output$e <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop (t, p) * input$rh / 100
      sprintf ('vapor pressure is %.3f hPa <br> &emsp; *** (enhancement factor included)', e)
    } else {
      e <- MurphyKoop(t) * input$rh / 100
    }
    ## save full numerical accuracy as global
    ewx <<- e
    er <- round(e,5)
    updateNumericInput(session, 'ewx', value=er)
    sprintf ('vapor pressure is %.3f hPa', e)
  })
  
  output$r <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p) * input$rh / 100
    } else {
      e <- MurphyKoop(t) * input$rh / 100
    }
    r <- MixingRatio (e/p) * 1000
    sprintf ('mixing ratio is %.3f g/kg', r)
  })
  
  output$dp <- renderText ({
    input$ewx
    if (exists ('ewx')) {
      e <- ewx
      rm ('ewx', pos='.GlobalEnv')
    } else {
      e <- input$ewx
    }
    p <- input$pressure
    if (e <= 0) {sprintf ('<br><br>DP is undefined')}
    else {
      if (input$ef) {
        dp <- nleqslv::nleqslv(10, function(x) MurphyKoop (x, p)-e)$x
      } else {
        dp <- nleqslv::nleqslv(10, function(x) MurphyKoop (x)-e)$x
      }
      sprintf ('<br><br>DP is %.3f C ', dp)
    }
  })
  
  output$q <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p) * input$rh / 100
    } else {
      e <- MurphyKoop(t)
    }
    r <- MixingRatio (e/p) * input$rh / 100
    q <- r/(1+r) * 1000
    sprintf ('specific humidity is %.3f g/kg', q)
  })
  
  output$MWA <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p) * input$rh / 100
    } else {
      e <- MurphyKoop(t) * input$rh / 100
    }
    eps <- StandardConstant('MWW') / (MWD <- StandardConstant('MWD'))
    Ma <- MWD * (1 + (eps - 1) * e / p)
    sprintf ('molec. weight: Ma=%.3f', Ma)
  })
  
  output$Ra <- renderText ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p) * input$rh / 100
    } else {
      e <- MurphyKoop(t) * input$rh / 100
    }
    eps <- StandardConstant('MWW') / (MWD <- StandardConstant('MWD'))
    Ma <- MWD * (1 + (eps - 1) * e / p)
    Ra <- StandardConstant('Ru') / Ma
    sprintf ('gas constant: Ra=%.3f', Ra)
  })
  
  output$composition <- renderTable ({
    t <- as.numeric(input$temp)
    p <- as.numeric(input$pressure)
    if (input$ef) {
      e <- MurphyKoop(t, p) * input$rh / 100
    } else {
      e <- MurphyKoop(t) * input$rh / 100
    }
    f <- c(0.78102, 0.20946, 0.00916, 0.00033, 0)
    f <- f * (1-e/p)
    f[5] <- e / p
    # print (sum(f))
    # Table <- data.frame (gas=c(expression('N'[2]), expression('O'[2]), expression('Ar'), 
    #   expression('CO'[2]), expression(paste('H'[2],'O',sep=''))), 
    Table <- data.frame (gas=c('nitrogen', 'oxygen', 'argon', 'carbon dioxide',
      'water'),
      fraction=f)  ##c(0.78102, 0.20946, 0.00916, 0.00033, 0))
    XTable <- xtable(Table, caption='mole fractions for moist air', digits=5)
    options (digits=5)
    XTable
  }, digits=5)
  
  observeEvent (input$plot_click, {
    # print (input$plot_click)
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
       xlab('dew point [C]') + ylab('water vapor pressure [hPa]') +
       scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3), #limits = c(xmin, xmax),
       labels = trans_format("log10", math_format(10^.x))) + 
       annotation_logticks(sides='l') + theme_WAC()
     # ggplotly(g)
     print (g)
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

