#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
# suppressMessages (suppressWarnings (
#   library(Ranadu, quietly=TRUE, warn.conflicts=FALSE))
# )
source ('./PlotWAC.R')
xp <- (-600:600)/100
xpp <- (0:1000)
ypp <- pnorm(xpp/500-1, sd=.3)

n <- 50000
X1R <- rnorm(n); X2R <- rnorm(n)
DX <- X2R - X1R
ddd <- (-600:600)/100
firstQ1 <- TRUE
firstQ2 <- TRUE
firstQ3 <- TRUE
firstQ4 <- TRUE
firstQ5 <- TRUE
firstQ6 <- TRUE
library(shinythemes)
ui <- fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Precision and Resolution Exercise"),
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('Objective',
      includeHTML('ResolutionA.html')
    ),
    tabPanel ('Resolution Meaning #1',
      includeHTML('ResolutionB.html')
    ),
    tabPanel ('Probability Distributions, Meaning #1',
      # Sidebar with a slider for separation between measurands
      sidebarLayout(
        sidebarPanel(
          includeHTML ('Instructions.html'),
          sliderInput("separation",
            "d=distance (standard deviations) between measurands:",
            min = 0,
            max = 6,
            value = 1,
            step=0.02,
            round=-1
          ), 
          radioButtons('conf', 'desired confidence limit',
            choices=c("50%"=0.675, "68.3%"=1, "95.4%"=2), selected=1, inline=TRUE)
          # sliderInput("conf", "std dev for conf limit",
          #             min=1, max=4, value=1, step=0.1)
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("resolutionPlot", height="460px"),
          includeHTML ('ResolutionC.html')
        )
      )
    ),
    # tabPanel ('alternate definition',
    #           includeHTML('ResolutionD.html')
    # ),
    tabPanel ('Illustration of Meaning #2',
      sidebarLayout(
        sidebarPanel(
          sliderInput("bits",
            "n: Number of bits = 2^n",
            min = 1,
            max = 8,
            value = 4),
          width=3
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distBins"),
          includeHTML('ResolutionE.html')
        )
      )
    ),
    tabPanel ('Summary',
      includeHTML('ResolutionF.html')
    ),
    tabPanel ('Short Quiz',
      includeHTML('ResolutionExerciseA.html'),
      fluidRow(column(10,numericInput('Q1', 'answer (within 0.1 sigma)', value=-1, width='250px'), offset=2)),
      includeHTML('ResolutionExerciseB.html'),
      fluidRow(column(10,numericInput('Q2', 'answer (within 0.1 sigma)', value=-1, width='250px'), offset=2)),
      includeHTML('ResolutionExerciseC.html'),
      fluidRow(column(10,numericInput('Q3', 'answer (within 5%)', value=-1,width='200px'), offset=2)),
      includeHTML('ResolutionExerciseD.html'),
      fluidRow(column(10, radioButtons('Q4', 'answer', choices=c('8','256','1024','no answer'), selected='no answer', inline=TRUE), offset=2)),
      includeHTML ('ResolutionExerciseE.html'),
      fluidRow(column(10,radioButtons('Q5', 'answer', choices=c('5.8 mV', '9.77 mV', '12.5 mV', 'no answer'), selected='no answer', inline=TRUE), offset=2)),
      includeHTML ('ResolutionExerciseF.html'),
      fluidRow(column(10,radioButtons('Q6', 'answer', choices=c('2.82 mV', '3.09 mV', '9.77 mV', '12.5 mV', 'no answer'), selected='no answer', inline=TRUE), offset=2))
      
    )
    # includeHTML ('ResolutionExercise.html')
  )
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  observeEvent(input$Q1, {
    if (!firstQ1) {
      # print (sprintf ('%f', input$Q1))
      if (abs(input$Q1-2.6) < 0.1) {
        showModal(modalDialog(title='Right!', 'The answer is 2.6', easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('The correct answer can be found by checking 68.3%, then moving the slider until P2=0.80.',
          '  [Hint: left-right arrows can move the slider a small amount.]'),
          easyClose = TRUE))
      }
    }
    firstQ1 <<- FALSE
  })
  observeEvent(input$Q2, {
    if (!firstQ2) {
      # print (sprintf ('%f', input$Q2))
      if (abs(input$Q2-5.2) < 0.1) {
        showModal(modalDialog(title='Right!', 'The answer is 5.2',easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('The correct answer can be found by checking 95.4%, then moving the slider until P2=0.95.'),
          easyClose = TRUE))
      }
    }
    firstQ2 <<- FALSE
  })
  observeEvent(input$Q3, {
    if (!firstQ3) {
      # print (sprintf ('%f', input$Q3))
      if (abs(input$Q3-42) < 5) {
        showModal(modalDialog(title='Right!', 'The answer is 42%',easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('The answer can be found by checking 50% and using d=1. Notice that the red vertical lines',
               ' are very close to the +/-d limits from the center of x1. The integration of',
               ' the regions *outside* +/-d then give about 0.60, so the fraction *inside* is 0.4.',
               ' This is only approximate because the limits are not exactly +/-d, but this will',
               ' give an answer within a few percent of the right answer.'),
          easyClose = TRUE))
      }
    }
    firstQ3 <<- FALSE
  })
  observeEvent(input$Q4, {
    if (!firstQ4) {
      # print (sprintf ('%s', input$Q4))
      if (input$Q4 == '1024') {
        showModal(modalDialog(title='Right!', 'The answer is 1024.',easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('Hint: what is 2**10? 0 is a possible result, as is 2**10-1.'),
          easyClose = TRUE))
      }
    }
    firstQ4 <<- FALSE
  })
  observeEvent(input$Q5, {
    if (!firstQ5) {
      # print (sprintf ('%s', input$Q5))
      if (input$Q5 == '5.8 mV') {
        showModal(modalDialog(title='Right!', 'The answer is about 5.8 mV',easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('Hint: The resolution is about twice the precision. See the next question.'),
          easyClose = TRUE))
      }
    }
    firstQ5 <<- FALSE
  })
  observeEvent(input$Q6, {
    if (!firstQ6) {
      # print (sprintf ('%s', input$Q6))
      if (input$Q6 == '2.82 mV') {
        showModal(modalDialog(title='Right!', 'The answer is 2.82 mV',easyClose=TRUE))
      } else {
        showModal(modalDialog(
          title = "--try again--",
          span('Hint: See the bottom of the "Illustration of Meaning #2" tab.'),
          easyClose = TRUE))
      }
    }
    firstQ6 <<- FALSE
  })
  
  output$resolutionPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    
    D <- input$separation
    CL <- as.numeric (input$conf)
    x1<-dnorm(xp+D/2); x2 <- dnorm(xp-D/2)
    y <- dnorm(xp+D/2, sd=sqrt(2))
    plotWAC(data.frame(xp, x1, x2, y), xlim=c(-5, 5), xlab=expression(paste('x [units ', sigma[x], ']')), 
      ylab='Gaussian probability distributions', legend.position='topleft', cex.lab=2)
    x2t <- x2
    x2t[(xp > -D/2-sqrt(2)*CL) & (xp < sqrt(2)*CL-D/2)] <- NA
    lineWAC(xp, x2t, col='darkgreen', lwd=2.5)
    text (4, 0.35, labels=sprintf('d=%.2f', D), cex=1.5)
    text (4, 0.30, labels=sprintf('P=%.3f', 1-pnorm(-D+CL*sqrt(2))+pnorm(-D-CL*sqrt(2))), cex=1.5)
    text (4, 0.25, labels=sprintf('P2=%.3f',1-pnorm(CL*sqrt(2), D, sqrt(2))+pnorm(-sqrt(2)*CL,D,sqrt(2))), cex=1.5)
    points (c(-D/2+sqrt(2)*CL, -D/2, -D/2-sqrt(2)*CL), rep(dnorm(sqrt(2)*CL, sd=sqrt(2)), 3), pch=20, col='red')
    # points (-D/2-sqrt(2), dnorm(-sqrt(2), sd=sqrt(2)), pch=20, col='red')
    
    arrows (c(-D/2-sqrt(2), -D/2), dnorm(-sqrt(2), sd=sqrt(2)), c(-D/2, -D/2+sqrt(2)), dnorm(-sqrt(2), sd=sqrt(2)), 
      length=.2, code=3, col='red', lty=4)
    lines(c(-D/2+sqrt(2)*CL, -D/2+sqrt(2)*CL), c(-1,1), col='red', lty=2)
    lines(c(-D/2-sqrt(2)*CL, -D/2-sqrt(2)*CL), c(-1,1), col='red', lty=2)
    points(-D/2+sqrt(2)*CL, dnorm(sqrt(2)*CL-D), pch=20, col='darkgreen')
    points(-D/2-sqrt(2)*CL, dnorm(-D-sqrt(2)*CL), pch=20, col='darkgreen')
    if (D > 0) {
      arrows(-D/2, 0.04, D/2, 0.04, length=min(D/2, 0.2), code=3, col='black', lty=4)
      text(0, 0.055, labels='d', cex=1.5)
    }
    
    y <- DX + D
    E <- (ecdf(y)(ddd))
    j <- which (ddd > 1.41)[1]
    P <- 1 - E[j]
    text (6, 0.75, labels=sprintf('D=%.1f', D), cex=1.5)
    text (6, 0.70, labels=sprintf('P=%.3f', P), cex=1.5)
    text (-0.7-D/2, 0.19, labels=expression(sqrt(2)), col='red', cex=1.5)
    abline(v=-D/2, col='blue', lty=2)
    abline (v=D/2, col='darkgreen', lty=2)
  })
  
  output$distBins <- renderPlot({
    plotWAC(xpp, ypp, xlab='Time', ylab='y', lwd=3, cex.lab=2)
    n <- input$bits
    m <- 2^n-1
    ypn <- round(ypp*m)/m
    lines(xpp, ypn, col='red', lwd=1.5)
    legend('topleft', legend=c('measurand', sprintf('%d-bit digitized', n)),
      lwd=c(3,1.5), col=c('blue', 'red'))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

