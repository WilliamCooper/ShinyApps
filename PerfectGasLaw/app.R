#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(plotly)
library(ggthemes)

Rd <- 287.0653
eps <- 0.6219958
TZERO <- 273.15
source("MurphyKoop.R")
source('PlotWAC.R')
source("theme_WAC.R")
airDensity <- function (p, TT) {
  return (p*100 / (Rd * (TT+273.15)))
}
adTemperature <- function (pref, Tref, eref=0, p) {
  RbyCp <- Rd/1004.728 / (1 + eref/(7*pref))
  return ((Tref+273.15)*(p/pref)^RbyCp - 273.15)
}
palt <- function (p) {
  x <- 0.1902633
  z <- 288.15/0.0065 * (1-(p/1013.25)^x)
  z[p < 226.3206] <- 11000 + 14602.12 * log10(226.3206/p[p < 226.3206])
  return(z)
}

p <- seq (40, 1000, by=10)
TC <- seq (-85, 40, by=5)
TV <- TC
e <- MurphyKoop(TC)

rho <- matrix(nrow=length(p), ncol=length(TC))
rhos <- matrix(nrow=length(p), ncol=length(TC))
hvr <- matrix(nrow=length(p), ncol=length(TC))
for (i in 1:length(p)) {
  for (j in 1:length(TC)) {
    rho[i,j] <- airDensity (p[i], TC[j])
    r <- eps * e[j] / (p[i] - e[j])
    TV[j] <- (TC[j] + 273.15) * (1 + r / eps) / (1 + r) - 273.15
    rhos[i,j] <- airDensity (p[i], TV[j])
    hvr[i,j] <- sprintf('%.3f %.3f', rho[i,j], rhos[i,j])
  }
}
fnt <- list(family = "Arial", size = 18, color = "#7f7f7f")
ylab <- list(title = "P", titlefont = fnt, showgrid=TRUE)
xlab <- list(title = "T", titlefont = fnt)
zlab <- list(title=" density", titlefont = fnt)

ui <- fluidPage(
  
  titlePanel("The Ideal-Gas Law: An Exercise"),
  tabsetPanel (id='whichTab', type='pills',
               tabPanel ('Objective',
                         includeHTML('PerfectGasA.html')
               ),
               tabPanel ('Ideal-Gas Law: 3D Display',
                         # Sidebar with a slider input for number of bins 
                         sidebarLayout(
                           sidebarPanel(
                             checkboxGroupInput('sfc', label='surfaces?', choices=c('dry air', 'humid air'), selected='dry air'),
                             sliderInput("rh",
                                         "relative humidity",
                                         min = 0,
                                         max = 100,
                                         value = 100),
                             includeHTML ('PerfectGas1.html')
                           ),
                           
                           mainPanel(
                             plotlyOutput("sfcPlot", height="650px")
                           )
                         )
               ),
               tabPanel ('Adiabatic Processes',
                         sidebarLayout(
                           sidebarPanel(
                             includeHTML ('PerfectGasb1.html'),
                             fluidRow(
                               column(6, numericInput(inputId='p1', label='P [hPa]', value=850, min=100, max=1000)),
                               column(6, numericInput(inputId='T1', label='T [degC]', value=15, min=-60, max=30))
                             ),
                             sliderInput("rh2",
                                         "relative humidity",
                                         min = 0,
                                         max = 100,
                                         value = 100),
                             includeHTML ('PerfectGasB.html')
                           ),

                           mainPanel(
                             plotOutput("adPlot", click=clickOpts(id="adPlotClick"), height="600px"),
                             includeHTML ('PerfectGasC.html')
                           )
                         )
               ),
               tabPanel ('Buoyancy',
                            includeHTML('PerfectGasD.html')
               ), 
               tabPanel ('The Standard Atmosphere',
                         sidebarLayout(
                           sidebarPanel(
                             includeHTML ('PerfectGasE.html')
                           ),
                           
                           mainPanel(
                             plotlyOutput("ISAPlot", height="600px"),
                             includeHTML ('PerfectGasF.html')
                           )
                         )
               )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  observeEvent(input$adPlotClick, {
    # print (input$adPlotClick)
    updateNumericInput (session, "T1", value=input$adPlotClick$x)
    updateNumericInput (session, "p1", value=input$adPlotClick$y)
  })
  
  output$sfcPlot <- renderPlotly({
    if ('humid air' %in% input$sfc) {
      rhum <- input$rh / 100
      for (i in 1:length(p)) {
        for (j in 1:length(TC)) {
          r <- eps * e[j] * rhum / (p[i] - rhum * e[j])
          TV[j] <- (TC[j] + 273.15) * (1 + r / eps) / (1 + r) - 273.15
          rhos[i,j] <- airDensity (p[i], TV[j])
        }
      }
    }
    if ('dry air' %in% input$sfc) {
      q <- plot_ly(z = ~rho, x = ~TC, y = ~p, type='surface', text=hvr, hoverinfo='text+x+y') 
      if ('humid air' %in% input$sfc) {
        q <- add_surface(q, z = ~rhos) 
      }
    } else if ('humid air' %in% input$sfc) {
      q <- plot_ly(z = ~rhos, x = ~TC, y = ~p, type='surface', text=hvr, hoverinfo='text+x+y') 
    }
    if (length(input$sfc) > 0) {
      layout(q, scene=list(xaxis=xlab, yaxis=ylab, zaxis=zlab, camera=list(eye=list(x=-1.30, y=-1.5, z=0.03))), 
             title='The Ideal-Gas Law') 
    }
  })
  
  output$adPlot <- renderPlot ({
    tmin <- -60
    tmax <- 30
    pmin <- 200
    pmax <- 1000
    pref <- input$p1
    Tref <- input$T1
    rhum <- input$rh2 / 100
    eref <- rhum * MurphyKoop(Tref)
    ps <- seq(pmin, pmax, by=10)
    ts1 <- adTemperature (pref, Tref, 0, ps)
    ts2 <- adTemperature (pref, Tref, eref, ps)
    ## now find where the temperature is lower than the dewpoint
    es <- MurphyKoop(ts2)
    ts2[es < eref * ps / pref] <- NA
    plotWAC(data.frame(T=ts1, P=ps), xlab=expression(paste('Temperature [',degree,' C]'), sep=''),
            ylab='pressure [hPa]', ylim=c(pmax, pmin), cex.lab=2)
    lines(ts2, ps, col='black', lwd=3)
    points(Tref, pref, pch=20, col='forestgreen', cex=3)
  })
  
  output$ISAPlot <- renderPlotly ({
    pmax <- 1025
    pmin <- 50
    P <- seq(pmin, pmax, by=1)
    zp <- palt(P)
    pk <- rep(0, 16)
    al <- list(rep(data.frame(), 11))
    T <- 15-zp*0.0065
    T[T < -56.5] <- -56.5
    TT <- c(T[1], T[length(T)])
    Z <-rep(' ', length(P))
    for (i in 1:length(P)) {
      Z[i] <- sprintf ('%.0f', zp[i]) 
    }
    DF <- data.frame (T, P, Z)
    q <- ggplot(data=DF) + geom_path(aes(x=T, y=P, label=Z),  color='blue', lwd=2)+ylim(pmax, pmin)
    for (i in c(1:6,8,10,12,14,16)) {
      pk[i] <- P[which(zp < i*1000)[1]]
      al[[i]] <- data.frame(TT, PP=c(pk[i], pk[i]))
      q <- q + geom_path(data=al[[i]], aes(x=TT, y=PP), col='forestgreen', lty=3)
      q <- q + annotate ("text", x=12, y=pk[i]-15, label=sprintf ('%d', i*1000))
    }
    pk0 <- P[which(zp < 0)[1]]
    al0 <- data.frame(TT, PP=c(pk0,pk0))
    q <- q + geom_path(data=al0, aes(x=TT, y=PP), col='forestgreen', lty=3)
    q <- q + annotate ("text", x=12, y=pk0-15, label='0')
    q <- q + xlab('Temperature [deg.C]') + ylab('Pressure [hPa]') + theme_WAC(1)
    ggplotly(q)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

