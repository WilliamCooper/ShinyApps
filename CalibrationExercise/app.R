#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## global stuff here
load('CalData.Rdata')
source('PlotWAC.R')
CalData <<- CalData
options("digits"=4)
SummarizeFit <- function(ft) {
  options("digits"=4)
  # print (summary(ft)$call)
  print ("Coefficients:")
  print (summary(ft)$coefficients)
  print (sprintf ("Residual standard deviation: %.3f, dof=%d<br>", summary(ft)$sigma, summary(ft)$df[2]))
  print (sprintf ("R-squared %.3f", summary(ft)$r.squared))
}
with (CalData, {
  fm1 <<- lm (M ~ x);
  fm2 <<- lm (x ~ M);
  fm3 <<- lm (M ~ x + I(x^2));
  fm4 <<- lm (x ~ M + I(M^2))  
})
cf1 <- coef(fm1)
cf2 <- coef(fm2)
cf3 <- coef(fm3)
cf4 <- coef(fm4)


ui <- fluidPage(
  # Application title
  titlePanel("Calibration Exercise"),
  tabsetPanel (id='whichTab', type='pills',
               tabPanel ('Objective',
                         includeHTML('CalibrationExerciseA.html')
               ), 
               tabPanel ('The Exercise',
                         includeHTML('CalibrationExerciseB.html'),
                         fluidRow (
                           column (2, numericInput ('m55', label='x for M=55:', value=10, step=0.01)),
                           column (3, wellPanel(textOutput('dummy'), textOutput('dummy2'), htmlOutput ('m55a')))
                         ),
                         fluidRow (
                           column (8, textInput ('fformula', label='formula: x = ', value='0.9+0.2*M+0.0001*M^2',
                                                 placeholder=' (0.9+0.2*M+0.0001*M**2     '))
                           # column (4, actionButton (inputId='checkIt', label='Check It:'))
                         ),
                         fluidRow (
                           column (4, textOutput ('chksum', container=pre)),
                           column (8, plotOutput ('showfit'))
                         )
               ), 
               tabPanel ('Polynomial-Fit Tool',
                         sidebarLayout(
                           sidebarPanel(
                             fluidRow (
                               column (6, actionButton (inputId='manual', label = 'More Info',
                                                        onclick ="window.open('https://drive.google.com/open?id=0B1kIUH45ca5AZWI5QllIdFpFR0U', '_blank')")),
                               column (6, checkboxInput('reverse', label='M=f(x)', value=FALSE))),
                             numericInput ('fitOrder', label='Degree of Polynomial', 
                                           min=1, max=5, step=1, value=1), 
                             includeHTML ('CalibrationExerciseInfo.html'),
                             
                             width=4
                           ),
                           
                           # Show a plot of the generated distribution
                           mainPanel(
                             plotOutput("calibrationPlot"),
                             htmlOutput ('fitSummary', container=pre)
                             # includeHTML ("TransferFunctionInfo.html"), width=6
                           )
                         )
               ),               
               tabPanel ('Discussion of Results',
                         includeHTML ('CalibrationExerciseC.html')
               ),
               tabPanel ('Lessons Learned',
                         includeHTML ('CalibrationExerciseD.html'),
                         column(6, plotOutput('hrplot', width="600px"))
                         ),
               tabPanel ('Notes',
                         includeHTML ('CalibrationExerciseE.html')
                         )
               
  )
)

server <- function(input, output, session) {
  
  reac <- reactiveValues (newfit=0, chkm55=0, updatefit=0)
  
  output$fitSummary <- renderPrint({
    reac$newfit
    SF <- readLines ('SummaryFile.txt')
    SF <- gsub ('"', '', SF)
    SF <- gsub ('\\[.\\] ', '', SF)
    print(SF)
  })
  
  output$m55a <- renderText ({
    reac$chkm55
    pcterror=abs(input$m55-11.96)/11.96 * 100
    if (pcterror < 0.3) {
      e <- paste(sprintf ('That\'s within %.1f%% - excellent!', 0.3),
      'That\'s about the best expected with this calibration.',
      sprintf('My answer is %.2f', 11.96), sep='<br>')
    } else if (pcterror < 1) {e <- paste(' ', sprintf ('That\'s within 1%% - not bad, but you can do better!'), ' ', sep='<br>')}
    else if (pcterror < 2) {e <- paste(' ', sprintf ('That\'s within 2%% - try again'), ' ', sep='<br>')}
    else if (pcterror < 5) {e <- paste(' ', sprintf ('That\'s within 5%% - but not very good!'), ' ', sep='<br>')}
    else {e <- paste(' ', sprintf ('That\'s more than 5%% in error - way off!'), ' ', sep='<br>')}
    # e <- gsub('[.]', '', e)
    e
  })
  
  output$chksum <- renderText ({
    reac$updatefit
    y <- NA
    M <- CalData$M
    e <- paste ('y <- ', input$fformula, sep='')
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <- sd (y - CalData$x)
      e <- sprintf ('the standard deviation is %.3f', sf)
      e
    }
  })
  
  observeEvent (input$checkIt, {
    e <- paste ('y <- ', input$fformula, sep='')
    print (sprintf ('in checkIt, e is %s', e))
    y <- NA
    M <- CalData$M
    try(eval (parse (text=e)), silent=TRUE)
    if (!is.na(y[1])) {
      sf <<- sd (y - CalData$x)
      print (sprintf ('sd=%.3f', sf))
    }
    isolate (reac$updatefit <- reac$updatefit + 1)
  })
  
  observeEvent (input$manual, seeManual ())

  output$hrplot <- renderPlot ({
    fm8 <- with(CalData, lm (x ~ MP + I(MP^2) + I(MP^3) + I(MP^4)))
    cf8 <- coef(fm8)
    x2 <- 1:30
    a1 <- 5; a2 <- 3; a3 <- 0.1
    y5a <- a1 + a2 * x2 + a3 * x2^2
    x8 <- cf8[1] + cf8[2]*y5a + cf8[3]*y5a^2 + cf8[4]*y5a^3 + cf8[5]*y5a^4
    plotWAC(data.frame(x=x2, M=y5a), type='l', col='blue', xlab='x', ylab='M', cex.lab=2)
    points (CalData$x, CalData$MP, pch=19, col='blue')
    lines (x8, y5a, col='red', lwd=2, lty=2)
    legend('bottomright', legend=c('cal points (100-pt ave.)', 'true calibration', '4th-degree fit to ave. data'),
           col=c('blue', 'blue', 'red'), lwd=c(NA,1,2), lty=c(NA,1,2), 
           pch=c(19,NA,NA), text.col=c('blue', 'blue', 'red'))
  })
  
  output$showfit <- renderPlot({
    plotWAC(data.frame(x=CalData$x, M=CalData$M), xlab='x', ylab='M', type='p', col='blue', cex.lab=2)
    y <- NA
    with (CalData, {
      e <- paste ('y <- ', input$fformula, sep='');
      suppressMessages (suppressWarnings (
        try (eval (parse (text=e)), silent=TRUE))
      );
      if (!is.na(y[1])) {lines (y, M, col='red', lwd=2)}
      # lines (y, z, col='red', lwd=2)
    })
    abline (h=55, col='forestgreen', lty=2)
  })
  
  output$calibrationPlot <- renderPlot({
    order <- input$fitOrder
    reverse <- input$reverse
    if (reverse) {
      e <- expression ('CalData$M ~ CalData$x')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$x^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$x^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$x^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$x^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$x^6)'))}
    } else {
      e <- expression ('CalData$x ~ CalData$M')
      if (order > 1) {e <- paste (e, expression (' + I(CalData$M^2)'))}
      if (order > 2) {e <- paste (e, expression (' + I(CalData$M^3)'))}
      if (order > 3) {e <- paste (e, expression (' + I(CalData$M^4)'))}
      if (order > 4) {e <- paste (e, expression (' + I(CalData$M^5)'))}
      if (order > 5) {e <- paste (e, expression (' + I(CalData$M^6)'))}
    }
    fm <- lm (eval(e))
    SummarizeFit (fm)
    sink (file='SummaryFile.txt')
    SummarizeFit (fm)
    sink (NULL)
    isolate(reac$newfit <- reac$newfit + 1)
    cf <- coef (fm)
    plotWAC(data.frame(x=CalData$x, M=CalData$M), xlab='x', ylab='M', type='p', pch=19, col='blue', cex.lab=2)
    if (reverse) {
      y <- 0
      z <- (0:500) * (max(CalData$x) - min(CalData$x)) / 500 + min(CalData$x)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}  
      lines (z, y, col='red', lwd=2)
    } else {
      y <- 0
      z <- (0:500) * (max(CalData$M) - min(CalData$M)) / 500 + min(CalData$M)
      for (i in 1:length(cf)) {y <- y + cf[i]*z^(i-1)}
      lines (y, z, col='red', lwd=2)
    }
    
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

