#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## clear global environment that might be left from the last run
rm(list=ls(all=TRUE))

library(shiny)
# library(Ranadu)
library(shinythemes)
library(ggplot2)
library(plotly)
library(ggthemes)

# suppressMessages (suppressWarnings (
#   library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
# )
source ('PlotWAC.R')
source ('ggplotWAC.R')
source ('theme_WAC.R')

seeManual <- function () {
  # viewer <- getOption ("viewer")
  # viewer ('TransferFunctionInfo.html')
}
# InfoHTML <- readLines ('TransferFunctionInfo.html')
firstQ1 <- TRUE
firstQ2 <- TRUE
firstQ3a <- TRUE
firstQ3b <- TRUE
makePlot <- function(t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, ylim) {
  # pdf(file='ResponsePlot.pdf', width=9, height=6)
  DP <- data.frame(Time=t, M1=M, M2=M2, x=x)
  DP <<- DP
  if (type == 'ramp' || type == 'step' || type == 'impulse') {
    # title(main=sprintf('omega=%.2f gamma %.02f', omega, gamma), cex.main=2, line=1)
    # v1 <- sprintf ("%0.2f, ", omega)
    # v2 <- sprintf ("%0.2f", gamma)
    # print (c(v1,v2))
    # ttl <- parse(text=c(expression (omega=),v1,expression(gamma=),v2))
    # ttl <- expression(paste(omega,'=',eval(sprintf('%.2f',omega)), sep=''))
    ttl <- substitute(
      paste(omega, "=", v1,', ',gamma,'=',v2),
      list(v1 = round(omega,1), v2=round(gamma,1)))
    # print(ttl)
    # title(main=ttl, cex.main=2, line=1)
    # q <- q + ggtitle(ttl)
  } else {
    ttl <- substitute(
      paste("frequency=",v3,', ',omega, "=", v1,', ',gamma,'=',v2),
      list(v1 = round(omega,1), v2=round(gamma,1), v3=round(freq,2)))
      # title(main=sprintf('frequency=%.2f, omega=%.2f gamma %.02f', freq, omega, gamma), cex.main=2, line=1)
      # title(main=ttl, cex.main=2, line=1, outer=TRUE)
      # q <- q + ggtitle(ttl)
  }
  labx <- expression(paste('Time / ',tau, sep=''))
  # labx <- 'Time / tau'
  plotWAC (DP[, c('Time','M1','M2', 'x')], xlab=expression(paste('Time / ',tau, sep='')), 
    ylab='x or M', cex.lab=2, col=c('blue', 'forestgreen', 'black'), lwd=c(2,2,3), 
    lty=c(1,1,2), ylim=ylim, legend.position='bottomright') 
  title(main=ttl, cex.main=1.5)
  # dev.off()
  # q <- plotWAC(DP, ylab="M or x", xlab=expression(paste("Time / ",tau)), 
  #   lwd=c(2,2,2), cex.lab=2, ylim=ylim, theme.version=1) 
  # return(q)
}
plotPNG <- function (omega, gamma, freq, TT, type, d1o, ylim) {
  tau <- 1
  kbym <- omega^2
  Dbym <- 2*gamma*omega
  N <- 2000
  dt <- (TT+1)/N
  t <- (1:N)*(TT+1)/N-1
  if (type == 'step') {
    x <- rep(1,N)
  } else if (type == 'ramp') {
    x <- t
  } else if (type == 'impulse') {
    x <- rep(0,N)
    x[t >= 0 & t < 0.1] <- 1
  } else if (type == 'sine wave') {
    x <- sin(2*pi*t*freq)
  } else if (type == 'square wave') {
    x <- sin(2*pi*t*freq)
    x <- ifelse (x >= 0, 1., -1.)
  } else if (type == 'triangle') {
    x <- asin (sin(2*pi*t*freq)) * 2 / pi
  }
  x[t <= 0] <- 0
  D1 <- ifelse (d1o, (log10 (gamma)+2)/4, 0)
  M <- M2 <- Mdot <- rep(0,N)
  for (i in 1:(N-1)) {
    H1 <- (1 - D1) * (x[i]-M[i])/tau
    M[i+1] <- M[i] + H1 * dt
    H2 <- ((x[i]-M2[i])*kbym-Dbym*Mdot[i])
    Mdot[i+1] <- Mdot[i] + H2 * dt
    M2[i+1] <- M2[i] + Mdot[i] * dt
  }
  png (file='TransferFunctionPlot.png', width=640, height=480)
  makePlot (t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, ylim)
  dev.off()
}
plotPDF <- function (omega, gamma, freq, TT, type, d1o, ylim) {
  tau <- 1
  kbym <- omega^2
  Dbym <- 2*gamma*omega
  N <- 2000
  dt <- (TT+1)/N
  t <- (1:N)*(TT+1)/N-1
  if (type == 'step') {
    x <- rep(1,N)
  } else if (type == 'ramp') {
    x <- t
  } else if (type == 'impulse') {
    x <- rep(0,N)
    x[t >= 0 & t < 0.1] <- 1
  } else if (type == 'sine wave') {
    x <- sin(2*pi*t*freq)
  } else if (type == 'square wave') {
    x <- sin(2*pi*t*freq)
    x <- ifelse (x >= 0, 1., -1.)
  } else if (type == 'triangle') {
    x <- asin (sin(2*pi*t*freq)) * 2 / pi
  }
  x[t <= 0] <- 0
  D1 <- ifelse (d1o, (log10 (gamma)+2)/4, 0)
  M <- M2 <- Mdot <- rep(0,N)
  for (i in 1:(N-1)) {
    H1 <- (1 - D1) * (x[i]-M[i])/tau
    M[i+1] <- M[i] + H1 * dt
    H2 <- ((x[i]-M2[i])*kbym-Dbym*Mdot[i])
    Mdot[i+1] <- Mdot[i] + H2 * dt
    M2[i+1] <- M2[i] + Mdot[i] * dt
  }
  pdf (file='TransferFunctionPlot.pdf', width=6, height=4)
  makePlot (t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, ylim)
  dev.off()
}

xp <- (-600:600)/100
n <- 50000
X1R <- rnorm(n); X2R <- rnorm(n)
DX <- X2R - X1R
ddd <- (-600:600)/100


ui <- fluidPage(#theme = shinytheme("united"),
  # Application title
  titlePanel("Transfer Function Exercise"),
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('Objective and Guidance',
      fluidPage(includeHTML('HTML/Objective.html'))),
    tabPanel ('Response Curves',
      sidebarLayout(
        sidebarPanel(
          # fluidRow (
          #   column(4, actionButton (inputId='manual', label = 'More Info',
          #               onclick ="window.open('https://drive.google.com/open?id=0B1kIUH45ca5AZDdyVmF4RmR5akU', '_blank')"))),
          # column(4, actionButton (inputId='plotPDF', label='PDF')),
          # column(4, actionButton (inputId='plotPNG', label='PNG'))),
          fluidRow (
            column (7, selectInput (inputId='Input', label='input waveform',
              choices=c('step', 'ramp', 'sine wave', 'square wave', 
                'impulse', 'triangle'), selected='step')),
            column (5, checkboxInput ('d1o', label='1st-order damping', value=FALSE))
              # checkboxInput ('ggplot', label='only ggplot2?', value=TRUE))
          ),
          sliderInput (inputId='freq', label='log freq (if wave)', 
            value=0, min=-2, max=2, step=0.01),
          sliderInput (inputId='omega', label='log omega (2nd order)', 
            value=0, min=-2, max=2, step=0.002),
          sliderInput (inputId='gamma', label='log gamma (2nd order)', 
            value=0, min=-2, max=2, step=0.002),
          sliderInput("ylim",
            "ordinate scale:",
            min = -10,
            max = 10,
            value = c(0,1),
            step=0.1,
            round=-1
          ),
          sliderInput("plotDuration",
            "duration to plot:",
            min = 1,
            max = 50,
            value = 5,
            step=0.1,
            round=-1
          )
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          textOutput('header'),
          plotlyOutput("transferPlot", height='600px'),
          plotOutput ('tPlot', height='600px')
          # verbatimTextOutput("event"),
          # includeHTML ("TransferFunctionInfo.html")
        )
      )
    # ),
    # tabPanel ('Short Quiz',
    #   includeHTML ("HTML/TFQ1.html"),
    #   fluidRow(column(10, numericInput('Q1', 'answer in seconds (within 10%)', value=0, width='250px'), offset=2)),
    #   includeHTML ('HTML/TFQ2.html'),
    #   fluidRow(column(10, numericInput('Q2', 'answer in radians/s (within 10%)', value=0, width='250px'), offset=2)),
    #   includeHTML ('HTML/TFQ3a.html'),
    #   fluidRow(column(10, radioButtons('Q3a', label='answer:', choices=c('1st','2nd'), width='250px', 
    #     selected=character(0), inline=TRUE), offset=2)),
    #   fluidRow(img(src='Q3.png', align = "left")),
    #   includeHTML ('HTML/TFQ3b.html'),
    #   fluidRow(column(10, numericInput('Q3b', 'answer in seconds (within 10%)', value=0, width='250px'), offset=2)),
    #   includeHTML ('HTML/TFQ3c.html'),
    #   fluidRow(column(10, radioButtons('Q3c', label='answer:', choices=c('overdamped', 'critically damped', 
    #     'underdamped, gamma about 0.7', 'strongly underdamped'), width='500px', selected=character(0), inline=TRUE), offset=2)),
    #   includeHTML ('HTML/TFQ3d.html'),
    #   fluidRow(column(10, radioButtons('Q3d', label='answer:', choices=c('0.1', '1', '5', '10'), width='250px',
    #     selected=character(0), inline=TRUE), offset=2))
    )
  )
)

server <- function(input, output, session) {
  
  Q1.d <- debounce (reactive (input$Q1), 2000)
  Q2.d <- debounce (reactive (input$Q2), 2000)
  observeEvent(Q1.d(), {
    if (!firstQ1) {
      # print (sprintf ('%f', input$Q1))
      Q1 <- Q1.d()  ##input$Q1
      if (abs(Q1 - 0.22) / 0.22 < 0.1) {
        showModal(modalDialog(title='Right!', 'The answer is 0.22 s', easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('Hint: With "step" selected, place cursor over blue line to find the time',
            ' corresponding to M1=0.90 [which should occur for t=-ln(0.1)]; the desired ',
            'time constant is 0.5 s divided by',
            ' this value.'),
          easyClose = TRUE))
      }
    }
    firstQ1 <<- FALSE
  })
  
  observeEvent(Q2.d(), {
    if (!firstQ2) {
      Q2 <- Q2.d()
      if (abs(Q2 - 2.1) / 2.1 < 0.1) {
        showModal(modalDialog(title='Right!', 'The answer is about 2.1. Try some smaller values of gamma to see how this decreases the lag.', easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('Leave the setting of γ = 1 and adjust the value of ω until the first-order and second-order response ',
            'lines overlap at long time. [Hint: you can make fine adjustments to the sliders by left-mouse-clicking ',
            'the slider button and then using the left-arrow or right-arrow keys.]'),
          easyClose = TRUE))
      }
    }
    firstQ2 <<- FALSE
  })
  
  observeEvent(input$Q3a, {
    if (input$Q3a == '2nd') {
      showModal(modalDialog(title='Right!', 'The response is not simple-exponential motion toward x.', easyClose=TRUE))
    } else {
      showModal(modalDialog(
        title = 'no, it is 2nd order',
        span('A first-order system always responds toward the current value of the input,',
          ' so it would not continue upward after the impulse function returns to zero.'),
        easyClose = TRUE))
    }
    firstQ3b <<- FALSE
  })
  
  observeEvent(input$Q3b, {
    if (!firstQ3b) {
      showModal(modalDialog(title='Error:', 'The system is not first order.', easyClose=TRUE))
    }
  }, priority=5)
  
  observeEvent(input$Q3c, {
    if (grepl('gamma', input$Q3c)) {
      showModal(modalDialog(title='Right!', 'The response overshoots just slightly past zero.', easyClose=TRUE))
    } else {
      showModal(modalDialog(
        title = 'no, try again',
        span('Consider the degree to which the response returns below zero before settling to zero.'),
        easyClose = TRUE))
    }
  })
  
  observeEvent(input$Q3d, {
    if ('10' == input$Q3d) {
      showModal(modalDialog(title='Right!', 'The amplitude of the upward response requires a high characteristic angular frequency.', easyClose=TRUE))
    } else {
      showModal(modalDialog(
        title = 'no, try again',
        span('Try this: Set gamma as required for the last question (0.7) and adjust omega to match',
          ' the amplitude of response shown in the figure for this question.'),
        easyClose = TRUE))
    }
  })
  
  observeEvent (input$manual, seeManual ())
  observeEvent (input$plotPDF, plotPDF (omega=10^input$omega, gamma=10^input$gamma, freq=10^input$freq,
    TT=input$plotDuration, type=input$Input, d1o=input$d1o,
    ylim=input$ylim))
  observeEvent (input$plotPNG, plotPNG (omega=10^input$omega, gamma=10^input$gamma, freq=10^input$freq,
    TT=input$plotDuration, type=input$Input, d1o=input$d1o,
    ylim=input$ylim))
  
  exprInput <- quote ({
    type <<- input$Input
    if (type == 'step' || type == 'ramp' || type == 'impulse') {
      updateNumericInput (session, 'ylim', value=c(0,1))
    } else {
      updateNumericInput (session, 'ylim', value=c(-1,1))
    }
  })
  obsInput <- observe (exprInput, quoted=TRUE)
  
  output$header <- renderText ({
    type <- input$Input
    if (type == 'ramp' || type == 'step' || type == 'impulse') {
      v1 <- paste(HTML('ω'),sprintf ('=%.2f', 10^input$omega),', ',HTML('γ'),sprintf('=%.2f', 10^input$gamma))
    } else {
      v1 <- paste(sprintf('frequency=%.2f, ', 10^input$freq), HTML('ω'),sprintf ('=%.2f', 10^input$omega),', ',HTML('γ'),sprintf('=%.2f', 10^input$gamma))
    }
  })
  
  output$transferPlot <- renderPlotly({
    tau <- 1
    omega <- 10^input$omega
    kbym <- omega^2
    gamma <- 10^input$gamma
    Dbym <- 2*gamma*omega
    N <- 5000
    TT <- input$plotDuration
    dt <- (TT+1)/N
    t <- (1:N)*(TT+1)/N-1
    type <<- input$Input
    if (type == 'step') {
      x <- rep(1,N)
    } else if (type == 'ramp') {
      x <- t
    } else if (type == 'impulse') {
      x <- rep(0,N)
      x[t >= 0 & t < 0.1] <- 1   
    } else if (type == 'sine wave') {
      freq <- 10^input$freq
      x <- sin(2*pi*t*freq)
    } else if (type == 'square wave') {
      freq <- 10^input$freq
      x <- sin(2*pi*t*freq)
      x <- ifelse (x >= 0, 1., -1.)
    } else if (type == 'triangle') {
      freq <- 10^input$freq
      x <- asin (sin(2*pi*t*freq)) * 2 / pi
    }
    x[t <= 0] <- 0
    D1 <- ifelse (input$d1o, (log10 (gamma)+2)/4, 0)
    M <- M2 <- Mdot <- rep(0,N)
    for (i in 1:(N-1)) {
      H1 <- (1 - D1) * (x[i]-M[i])/tau
      M[i+1] <- M[i] + H1 * dt
      H2 <- ((x[i]-M2[i])*kbym-Dbym*Mdot[i])
      Mdot[i+1] <- Mdot[i] + H2 * dt
      M2[i+1] <- M2[i] + Mdot[i] * dt
    }
    # plot(t,M, type='l', lwd=2, col='blue')
    # lines(t,N, col='darkorange',lty=1) 
    
    # makePlot (t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, input$ylim)
    ylim <- input$ylim
    # makePlot <- function(t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, ylim) {
    DP <- data.frame(Time=t, M1=M, M2=M2, x=x)
    DP <<- DP
    if (type == 'ramp' || type == 'step' || type == 'impulse') {
      # title(main=sprintf('omega=%.2f gamma %.02f', omega, gamma), cex.main=2, line=1)
      # v1 <- sprintf ("%0.2f, ", omega)
      # v2 <- sprintf ("%0.2f", gamma)
      # print (c(v1,v2))
      # ttl <- parse(text=c(expression (omega=),v1,expression(gamma=),v2))
      # ttl <- expression(paste(omega,'=',eval(sprintf('%.2f',omega)), sep=''))
      ttl <- substitute(
        paste(omega, "=", v1,', ',gamma,'=',v2),
        list(v1 = round(omega,3), v2=round(gamma,3)))
      ttl <- sprintf('omega = %.2f, gamma = %.2f', omega, gamma)
      # print(ttl)
      # title(main=ttl, cex.main=2, line=1)
      # q <- q + ggtitle(ttl)
    } else {
      ttl <- substitute(
        paste("frequency=",v3,', ',omega, "=", v1,', ',gamma,'=',v2),
        list(v1 = round(omega,3), v2=round(gamma,3), v3=round(freq,1)))
      ttl <- sprintf ('frequency=%.2f omega=%.2f gamma=%.2f', freq, omega, gamma)
      # title(main=sprintf('frequency=%.2f, omega=%.2f gamma %.02f', freq, omega, gamma), cex.main=2, line=1)
      # title(main=ttl, cex.main=2, line=1, outer=TRUE)
      # q <- q + ggtitle(ttl)
    }
    labx <- expression(paste('Time / ',tau, sep=''))
    labx <- 'Time / tau'
    q <- ggplot(data=DP, aes(x=Time)) + geom_path (aes(y=M1, color="M1")) + 
      geom_path (aes(y=M2, color="M2")) + geom_path (aes(y=x, color="x")) +
      # ggtitle(ttl) + 
      xlab (labx) + ylab('x or M') + ylim (ylim)
    q <- q + scale_colour_manual(name='lines', values=c('x'='black', 'M1'='blue', 'M2'='forestgreen')) + theme_WAC(0)
    # q <- ggplotWAC (DP[, c('Time','M1','M2', 'x')], 
    #   ylab='x or M', cex.lab=2, lwd=c(2,2,2), legend.position=c(0.9,0.1), gtitle=ttl) + xlab(labx)
    # title(ttl)
    # q <- plotWAC(DP, ylab="M or x", xlab=expression(paste("Time / ",tau)), 
    #   lwd=c(2,2,2), cex.lab=2, ylim=ylim, theme.version=1) 
    # return(q)
    # Q <<- plotly_build(q)
    ggplotly(q)
  })
  
  output$tPlot <- renderPlot({
    # if (input$ggplot) {return()}
    tau <- 1
    omega <- 10^input$omega
    kbym <- omega^2
    gamma <- 10^input$gamma
    Dbym <- 2*gamma*omega
    N <- 5000
    TT <- input$plotDuration
    dt <- (TT+1)/N
    t <- (1:N)*(TT+1)/N-1
    type <<- input$Input
    if (type == 'step') {
      x <- rep(1,N)
    } else if (type == 'ramp') {
      x <- t
    } else if (type == 'impulse') {
      x <- rep(0,N)
      x[t >= 0 & t < 0.1] <- 1   
    } else if (type == 'sine wave') {
      freq <- 10^input$freq
      x <- sin(2*pi*t*freq)
    } else if (type == 'square wave') {
      freq <- 10^input$freq
      x <- sin(2*pi*t*freq)
      x <- ifelse (x >= 0, 1., -1.)
    } else if (type == 'triangle') {
      freq <- 10^input$freq
      x <- asin (sin(2*pi*t*freq)) * 2 / pi
    }
    x[t <= 0] <- 0
    D1 <- ifelse (input$d1o, (log10 (gamma)+2)/4, 0)
    M <- M2 <- Mdot <- rep(0,N)
    for (i in 1:(N-1)) {
      H1 <- (1 - D1) * (x[i]-M[i])/tau
      M[i+1] <- M[i] + H1 * dt
      H2 <- ((x[i]-M2[i])*kbym-Dbym*Mdot[i])
      Mdot[i+1] <- Mdot[i] + H2 * dt
      M2[i+1] <- M2[i] + Mdot[i] * dt
    }
    # plot(t,M, type='l', lwd=2, col='blue')
    # lines(t,N, col='darkorange',lty=1) 
    
    # makePlot (t, M, M2, x, tau, type, omega, v1, gamma, v2, v3, freq, input$ylim)
  })
  
  
  # output$event <- renderPrint({
  #   d <- event_data("plotly_hover")
  #   if (is.null(d)) "Hover on a point!" else d
  # })
}

# Run the application 
shinyApp(ui = ui, server = server)

