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
library(shinythemes)
ui <- fluidPage(theme = shinytheme("cerulean"),
  # Application title
  titlePanel("Precision and Resolution Exercise"),
  tabsetPanel (id='whichTab', type='pills',
               # tabPanel ('Objective',
               #           includeHTML('ResolutionA.html')
               # ), 
               # tabPanel ('Resolution meaning #1',
               #           includeHTML('ResolutionB.html')
               #           ),
               tabPanel ('Probability Distributions, Meaning #1',
                         # Sidebar with a slider for separation between measurands
                         sidebarLayout(
                           sidebarPanel(
                             # checkboxInput ('makePDF', 'save as PDF'),
                             sliderInput("separation",
                                         "d=distance (standard deviations) between measurands:",
                                         min = 0,
                                         max = 6,
                                         value = 1,
                                         step=0.02,
                                         round=-1
                             ), 
                             radioButtons('conf', 'desired confidence limit',
                                          choices=c("50%"=0.675, "68.3%"=1, "95.4%"=2), selected=1, inline=TRUE),
                             # sliderInput("conf", "std dev for conf limit",
                             #             min=1, max=4, value=1, step=0.1)
                             includeHTML ('Instructions.html')
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("resolutionPlot", height="300px"),
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
               )  #,
               # tabPanel ('Summary',
               #           includeHTML('ResolutionF.html')
               # )
               
  )
  # includeHTML ('ResolutionExercise.html')
)
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$resolutionPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    # x    <- faithful[, 2] 
    # bins <- seq(min(x), max(x), length.out = input$bins + 1)
    # 
    makePDF <- FALSE
    # makePDF <- input$makePDF
    if (makePDF) {pdf(file='NewPlots/ResolutionPlot.pdf', width=9, height=6)}
    D <- input$separation
    CL <- as.numeric (input$conf)
    x1<-dnorm(xp+D/2); x2 <- dnorm(xp-D/2)
    y <- dnorm(xp+D/2, sd=sqrt(2))
    plotWAC(data.frame(xp, x1, x2, y), xlim=c(-5, 5), xlab=expression(paste('x [units ', sigma[x], ']')), 
            ylab='Gaussian probability distributions', legend.position='topleft', 
            col=c('blue', 'forestgreen', 'black'), lty=c(1,1,4), lwd=c(2,1,3), cex.lab=1.3)
    x2t <- x2
    x2t[(xp > -D/2-sqrt(2)*CL) & (xp < sqrt(2)*CL-D/2)] <- NA
    lineWAC(xp, x2t, col='forestgreen', lwd=2.5)
    text (4, 0.35, labels=sprintf('d=%.2f', D), cex=1.5)
    text (4, 0.30, labels=sprintf('P=%.3f', 1-pnorm(-D+CL*sqrt(2))+pnorm(-D-CL*sqrt(2))), cex=1.5)
    text (4, 0.25, labels=sprintf('P2=%.3f',1-pnorm(CL*sqrt(2), D, sqrt(2))+pnorm(-sqrt(2)*CL,D,sqrt(2))), cex=1.5)
    points (c(-D/2+sqrt(2)*CL, -D/2, -D/2-sqrt(2)*CL), rep(dnorm(sqrt(2)*CL, sd=sqrt(2)), 3), pch=20, col='black')
    # points (-D/2-sqrt(2), dnorm(-sqrt(2), sd=sqrt(2)), pch=20, col='red')
    
    arrows (c(-D/2-sqrt(2), -D/2), dnorm(-sqrt(2), sd=sqrt(2)), c(-D/2, -D/2+sqrt(2)), dnorm(-sqrt(2), sd=sqrt(2)), 
            length=.2, code=3, col='black', lty=2)
    lines(c(-D/2+sqrt(2)*CL, -D/2+sqrt(2)*CL), c(-1,1), col='black', lty=2)
    lines(c(-D/2-sqrt(2)*CL, -D/2-sqrt(2)*CL), c(-1,1), col='black', lty=2)
    points(-D/2+sqrt(2)*CL, dnorm(sqrt(2)*CL-D), pch=20, col='forestgreen')
    points(-D/2-sqrt(2)*CL, dnorm(-D-sqrt(2)*CL), pch=20, col='forestgreen')
    if (D > 0) {
      arrows(-D/2, 0.04, D/2, 0.04, length=min(D/2, 0.2), code=3, col='black', lty=2)
      text(0, 0.055, labels='d', cex=1.5)
    }
    
    y <- DX + D
    E <- (ecdf(y)(ddd))
    j <- which (ddd > 1.41)[1]
    P <- 1 - E[j]
    text (6, 0.75, labels=sprintf('D=%.1f', D), cex=1.5)
    text (6, 0.70, labels=sprintf('P=%.3f', P), cex=1.5)
    text (-0.7-D/2, 0.19, labels=expression(sqrt(2)), col='black', cex=1.5)
    abline(v=-D/2, col='blue', lty=2, lwd=2)
    abline (v=D/2, col='forestgreen', lty=2, lwd=2)
    # if (makePDF) {dev.off()}
  })
  
  output$distBins <- renderPlot({
    plotWAC(xpp, ypp, xlab='Time', ylab='y', lwd=3, cex.lab=2)
    n <- input$bits
    m <- 2^n-1
    ypn <- round(ypp*m)/m
    lines(xpp, ypn, col='forestgreen', lwd=1.5)
    legend('topleft', legend=c('measurand', sprintf('%d-bit digitized', n)),
           lwd=c(3,1.5), col=c('blue', 'forestgreen'))
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

