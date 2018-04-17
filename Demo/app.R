#
# This is a Shiny web application that demonstrates some uses of Shiny apps. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
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
source('../Resolution/PlotWAC.R')

ui <- fluidPage(
  
  # Application title
  titlePanel("Demonstration for EOL Forum 16 Apr 2018"),
  tabsetPanel (id='whichTab', type='pills',
               tabPanel ('Guide',
                         includeHTML ('HTML/Information.html')),
               tabPanel ('Background',
                       fluidRow (
                         column (3, numericInput ('I1frame', label='frame #',1,
                           min=1, max=7, step=1)),
                         column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                           'use the up/down arrow keys to step through the frames.'))),
                       imageOutput('IS1png')
                 ),
               tabPanel ('Instructional and Tutorial Examples',
                         tabsetPanel (id='whichBg', type='pills',
                                      tabPanel ('SEGUE Examples',
                                                tabsetPanel (id='whichsugg', type='pills',
                                                             tabPanel("Calibration Exercise",
                                                               includeHTML('HTML/Calibration.html')),
                                                             #          tabsetPanel (id='whichCE', type='pills',
                                                             #                       tabPanel ('the exercise',
                                                             #                                 includeHTML('CalibrationExercise/CalibrationExerciseA.html')
                                                             #                       ),
                                                             #                       tabPanel ('your answer',
                                                             #                                 includeHTML('CalibrationExercise/CalibrationExerciseB.html'),
                                                             #                                 fluidRow (
                                                             #                                   column (4, numericInput ('m55', label='x for M=55:', value=0)),
                                                             #                                   column (8, textOutput ('m55a', container=pre))
                                                             #                                 ),
                                                             #                                 fluidRow (
                                                             #                                   column (8, textInput ('fformula', label='formula: x = ', value='0.9+0.2*M+0.0001*M^2',
                                                             #                                                         placeholder=' (0.9+0.2*M+0.0001*M**2     '))
                                                             #                                   # column (4, actionButton (inputId='checkIt', label='Check It:'))
                                                             #                                 ),
                                                             #                                 fluidRow (
                                                             #                                   column (4, textOutput ('chksum', container=pre)),
                                                             #                                   column (8, plotOutput ('showfit'))
                                                             #                                 )
                                                             #                       ),
                                                             #                       tabPanel ('help with fitting',
                                                             #                                 sidebarLayout(
                                                             #                                   sidebarPanel(
                                                             #                                     fluidRow (
                                                             #                                       column (6, actionButton (inputId='manual', label = 'More Info',
                                                             #                                                                onclick ="window.open('https://drive.google.com/open?id=0B1kIUH45ca5AZWI5QllIdFpFR0U', '_blank')")),
                                                             #                                       column (6, checkboxInput('reverse', label='M=f(x)', value=FALSE))),
                                                             #                                     numericInput ('fitOrder', label='Order of Polynomial',
                                                             #                                                   min=1, max=5, step=1, value=1),
                                                             #                                     includeHTML ('CalibrationExercise/CalibrationExerciseInfo.html'),
                                                             #                                     
                                                             #                                     width=4
                                                             #                                   ),
                                                             #                                   
                                                             #                                   # Show a plot of the generated distribution
                                                             #                                   mainPanel(
                                                             #                                     plotOutput("calibrationPlot"),
                                                             #                                     htmlOutput ('fitSummary', container=pre)
                                                             #                                     # includeHTML ("TransferFunctionInfo.html"), width=6
                                                             #                                   )
                                                             #                                 )
                                                             #                       ),
                                                             #                       tabPanel ('our solution',
                                                             #                                 includeHTML ('CalibrationExercise/CalibrationExerciseC.html')
                                                             #                       ),
                                                             #                       tabPanel ('more',
                                                             #                                 includeHTML ('CalibrationExercise/CalibrationExerciseD.html'),
                                                             #                                 column(6, plotOutput('hrplot'))
                                                             #                       ),
                                                             #                       tabPanel ('notes',
                                                             #                                 includeHTML ('CalibrationExercise/CalibrationExerciseE.html')
                                                             #                       )
                                                             #                       
                                                             #          )
                                                             # ),
                                                             tabPanel ('Ideal-Gas Law',
                                                                       includeHTML('HTML/IdealGas.html')
                                                             ),
                                                             tabPanel ('Resolution',
                                                                       tabsetPanel (id='whichRes', type='pills',
                                                                                    tabPanel ('objective',
                                                                                              includeHTML('Resolution/ResolutionA.html')
                                                                                    ),
                                                                                    tabPanel ('explore def. 1',
                                                                                              includeHTML('Resolution/ResolutionB.html')
                                                                                    ),
                                                                                    tabPanel ('show PDFs',
                                                                                              # Sidebar with a slider for separation between measurands
                                                                                              sidebarLayout(
                                                                                                sidebarPanel(
                                                                                                  sliderInput("separation",
                                                                                                              "d=distance (units of sigma) between measurands:",
                                                                                                              min = 0,
                                                                                                              max = 5,
                                                                                                              value = 1,
                                                                                                              step=0.1,
                                                                                                              round=-1
                                                                                                  ),
                                                                                                  sliderInput("conf", "std dev for conf limit",
                                                                                                              min=1, max=4, value=1, step=0.1)
                                                                                                ),
                                                                                                
                                                                                                # Show a plot of the generated distribution
                                                                                                mainPanel(
                                                                                                  plotOutput("resolutionPlot"),
                                                                                                  includeHTML ('Resolution/ResolutionC.html')
                                                                                                )
                                                                                              )
                                                                                    ),
                                                                                    tabPanel ('alternate definition',
                                                                                              includeHTML('Resolution/ResolutionD.html')
                                                                                    ),
                                                                                    tabPanel ('meaning #2',
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
                                                                                                  includeHTML('Resolution/ResolutionE.html')
                                                                                                )
                                                                                              )
                                                                                    ),
                                                                                    tabPanel ('summary',
                                                                                              includeHTML('Resolution/ResolutionF.html')
                                                                                    ),
                                                                         tabPanel ('Short Quiz',
                                                                           includeHTML('Resolution/ResolutionExerciseA.html'),
                                                                           fluidRow(column(10,numericInput('Q1', 'answer (within 0.1 sigma)', value=-1, width='250px'), offset=2)),
                                                                           includeHTML('Resolution/ResolutionExerciseB.html'),
                                                                           fluidRow(column(10,numericInput('Q2', 'answer (within 0.1 sigma)', value=-1, width='250px'), offset=2)),
                                                                           includeHTML('Resolution/ResolutionExerciseC.html'),
                                                                           fluidRow(column(10,numericInput('Q3', 'answer (within 5%)', value=-1,width='200px'), offset=2)),
                                                                           includeHTML('Resolution/ResolutionExerciseD.html'),
                                                                           fluidRow(column(10, radioButtons('Q4', 'answer', choices=c('8','256','1024','no answer'), selected='no answer', inline=TRUE), offset=2)),
                                                                           includeHTML ('Resolution/ResolutionExerciseE.html'),
                                                                           fluidRow(column(10,radioButtons('Q5', 'answer', choices=c('5.8 mV', '9.77 mV', '12.5 mV', 'no answer'), selected='no answer', inline=TRUE), offset=2)),
                                                                           includeHTML ('Resolution/ResolutionExerciseF.html'),
                                                                           fluidRow(column(10,radioButtons('Q6', 'answer', choices=c('2.82 mV', '3.09 mV', '9.77 mV', '12.5 mV', 'no answer'), selected='no answer', inline=TRUE), offset=2))
                                                                           
                                                                         )
                                                                       )
                                                                       
                                                             ),
                                                             tabPanel ('Transfer Functions',
                                                                       includeHTML('HTML/TransferFunction.html')
                                                             ),
                                                             tabPanel ('Geopotential Height',
                                                                       includeHTML('HTML/GeopotentialHeight.html')
                                                             ),
                                                             tabPanel ('Water Vapor Pressure',
                                                                       includeHTML('HTML/VaporPressure.html')
                                                             ),
                                                             tabPanel ('Sensor Diaphragm',
                                                                       includeHTML('HTML/SensorDiaphragm.html')
                                                             ),
                                                             tabPanel ('Gust Probe',
                                                                       includeHTML('HTML/GustProbe.html')
                                                             ),
                                                             tabPanel ('...')
                                                )
                                      ),

                                      tabPanel ('R sessions (tutorial)',
                                                tabsetPanel (id='whichTab', type='pills',
                                                             tabPanel ('TOC',
                                                                       includeHTML('~cooperw/RStudio/RSessions/RSessions/TOC/TOC.html')),
                                                             tabPanel ('Getting Started',
                                                                       tabsetPanel (id='S1tab', type='pills',
                                                                                    tabPanel ('Getting Started',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session1/Session1a.html')),
                                                                                    tabPanel ('RStudio Tour',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session1/Session1b.html')),
                                                                                    tabPanel ('Some Examples',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session1/Session1c.html'),
                                                                                              tabsetPanel (id='S1ex', type='pills',
                                                                                                           tabPanel ('simple plot',
                                                                                                                     includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/E1Code.html'),
                                                                                                                     sidebarLayout(
                                                                                                                       sidebarPanel(
                                                                                                                         selectInput (inputId='S1Var', label='variable to plot', 
                                                                                                                                      choices=c('Temperature'='ATX',
                                                                                                                                                'Wind Speed'='WSC',
                                                                                                                                                'Pressure'='PSXC'))
                                                                                                                       ),
                                                                                                                       mainPanel(
                                                                                                                         plotOutput ('S1E1Plot')
                                                                                                                       )
                                                                                                                     )),
                                                                                                           tabPanel ('sounding',
                                                                                                                     includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/E2Code.html'),
                                                                                                                     plotOutput ('S1E2Plot', width="50%")),
                                                                                                           tabPanel ('stats',
                                                                                                                     includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/E3Code.html'),
                                                                                                                     dataTableOutput ('S1Stats')),
                                                                                                           tabPanel ('recovery factor',
                                                                                                                     includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/E4Code.html'))
                                                                                              )
                                                                                    ),
                                                                                    tabPanel ('Text-with-Code',
                                                                                              includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/Session1d.html')),
                                                                                    tabPanel ('Getting Ranadu',
                                                                                              includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session1/Session1e.html')))),
                                                             tabPanel ('Objects and the data.frame',
                                                                       tabsetPanel (id='S2tab', type='pills',
                                                                                    tabPanel ('Vectors and Matrices',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session2/Session2a.html'),
                                                                                              htmlOutput('txtS2a'),
                                                                                              radioButtons ('selS2a', label=NULL, choices=c(
                                                                                                'select a button below'=1,
                                                                                                'a <- 1:12; print(a)'=2,
                                                                                                'dim(a) <- c(3,4); print(a)'=3,
                                                                                                'print (t(a))'=4
                                                                                              ), width='400px')
                                                                                    ),
                                                                                    tabPanel ('The data.frame',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session2/Session2b.html')),
                                                                                    tabPanel ('Addressing and Subsetting data.frames',
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session2/Session2c1.html'),
                                                                                              radioButtons ('selS2c1', label=NULL, choices=c(
                                                                                                'select a button below'=1,
                                                                                                'Data$ATX[5]'=2, 
                                                                                                'Data[5, 2]'=3,
                                                                                                'Data[5, ]'=4, 
                                                                                                'Data[5, "ATX"]'=5, 
                                                                                                'Data$ATX'=6,
                                                                                                'attach(Data); ATX[5]'=7,
                                                                                                'with(Data, print(ATX[5])'=8
                                                                                              ), width='800px'),
                                                                                              htmlOutput ('txtS2c1'),
                                                                                              includeHTML('~cooperw/RStudio/RSessions/RSessions/Session2/Session2c2.html'),
                                                                                              actionButton ('XS2a', label='See an answer')
                                                                                    ),
                                                                                    tabPanel ('Some Basic Operations',
                                                                                              includeHTML ('~cooperw/RStudio/RSessions/RSessions/Session2/Session2d.html')))),
                                                             tabPanel ('Basics',
                                                                       tabsetPanel (id='S3tab', type='pills',
                                                                                    tabPanel ('R as a Calculator',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3aframe', label='frame #',
                                                                                                                         2, min=1, max=3, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3apng')
                                                                                    ),
                                                                                    tabPanel ('Basic Operators and Precedence',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3bframe', label='frame #',4,
                                                                                                                         min=4, max=12, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3bpng')
                                                                                    ),
                                                                                    tabPanel ('Vectorized Operations',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3cframe', label='frame #',13,
                                                                                                                         min=13, max=16, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3cpng')
                                                                                    ),
                                                                                    tabPanel ('Using Variables',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S3dframe', label='frame #',17,
                                                                                                                         min=17, max=21, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS3dpng')
                                                                                    )
                                                                       )
                                                             ),
                                                             tabPanel ('R packages',
                                                                       tabsetPanel (id='S4tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S4frame', label='frame #',2,
                                                                                                                         min=1, max=11, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS4png')
                                                                                    ))),
                                                             tabPanel ('Plotting',
                                                                       tabsetPanel (id='S5tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S5frame', label='frame #',2,
                                                                                                                         min=1, max=15, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS5png')
                                                                                    ),
                                                                                    tabPanel ('Base Graphics',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S5aframe', label='frame #',3,
                                                                                                                         min=3, max=9, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS5apng')
                                                                                    ),
                                                                                    tabPanel ('plotWAC',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S5bframe', label='frame #',8,
                                                                                                                         min=8, max=9, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS5bpng')
                                                                                    ),
                                                                                    tabPanel ('ggplot',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S5cframe', label='frame #',10,
                                                                                                                         min=10, max=15, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS5cpng')
                                                                                    )
                                                                       )
                                                             ),
                                                             tabPanel ('Fitting',
                                                                       tabsetPanel (id='S6tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S6frame', label='frame #',2,
                                                                                                                         min=1, max=21, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS6png')
                                                                                    ))
                                                             ),
                                                             tabPanel ('Reproducible Research',
                                                                       tabsetPanel (id='S7tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S7frame', label='frame #',2,
                                                                                                                         min=1, max=13, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box, then',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS7png')
                                                                                    ))
                                                             ),
                                                             tabPanel ('Data Review',
                                                                       tabsetPanel (id='S8tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S8frame', label='frame #',2,
                                                                                                                         min=1, max=13, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box',
                                                                                                                     'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS8png')
                                                                                    ))
                                                             ),
                                                             tabPanel ('Shiny apps',
                                                                       tabsetPanel (id='S9tab', type='pills',
                                                                                    tabPanel ('All',
                                                                                              fluidRow (
                                                                                                column (3, numericInput ('S9frame', label='frame #', 2,
                                                                                                                         min=1, max=4, step=1)),
                                                                                                column (4, helpText ('Suggestion: Click cursor in "frame #" entry box',
                                                                                                                      'use the up/down arrow keys to step through the frames.'))),
                                                                                              imageOutput('RS9png') 
                                                                                              # imageOutput('RS9png')
                                                                                    ))
                                                             )
                                                )
                                      ),
                                      tabPanel ('Tutorial, Variance Spectra',
                                                includeHTML('HTML/VarSpec.html')
                                                )
                         )
               ),
               tabPanel('Data Perusal and QA', 
                        tabsetPanel (id='DAQA', type='pills',
                                     tabPanel('Ranadu',
                                              includeHTML('HTML/Ranadu.html')
                                              ),
                                     tabPanel('QAtools',
                                              includeHTML('HTML/QAtools.html')
                                              )
                        )
               ),
               tabPanel('Special Functions',
                        tabsetPanel (id='spcl', type='pills',
                                     tabPanel('Kalman Filter'),
                                     tabPanel('Vertical Wind Options'),
                                     tabPanel('Terrain Height')
                        )
                        
               ),
               
               # Show a plot of the generated distribution
               mainPanel(
               )
  )
)

# Define server logic 
server <- function(input, output) {
  ## RSessions stuff:
  output$S1E1Plot <- renderPlot ({
    V <- input$S1Var
    nm <- c('Temperature [deg. C]', 'Wind Speed [m/s]', 'Pressure [hPa]')
    names (nm) <- c('ATX', 'WSC', 'PSXC')    ## these are the actual variables in the data file
    # Data <- getNetCDF('/Data/DEEPWAVE/DEEPWAVErf20.nc', c('ATX', 'WSC', 'PSXC'))
    load ('~cooperw/RStudio/RSessions/RSessions/Session1/Data.Rdata')
    plot (Data$Time, Data[, V], type='l', col='blue', lwd=2, xlab='Time [UTC]', ylab=nm[V])
    title ("DEEPWAVE flight 20")
    # with (Data, plotWAC (data.frame (Time, Data[, V]), ylab=nm[V]))
  })
  
  output$S1E2Plot <- renderPlot ({
    # Directory <- DataDirectory ()    # for portability; sets the local data directory
    # Flight <- "rf20"                 # select a flight
    # Project = "DEEPWAVE"             # select a project
    # fname = sprintf("%s%s/%s%s.nc", Directory,Project,Project,Flight)
    # # XXX set variables needed, here a standard list including DPX and EWX
    # # preliminary look shows that final descent was from 84400 to 91100
    # Data <- getNetCDF (fname, c("Time", "DPXC", "ATX", "PALT"), 84400, 91100)
    saveDataFile <- '~cooperw/RStudio/RSessions/RSessions/Session1/Data2.RData'
    # save (Data, file = saveDataFile) 
    # for future runs, it will be much faster to use:
    load(saveDataFile)
    plot (Data$DPXC, Data$PALT, type='l', lwd=1.5, # type='l': line plot
          xlab='Temperature or Dew Point [deg C]', ylab='pressure altitude [m]')   
    lines (Data$ATX, Data$PALT, col='forestgreen', lwd=2) # add temperature
    s <- Data$DPXC > Data$ATX
    lines (Data$DPXC[s], Data$PALT[s], col='red', lwd=3)
    # will show how to add legends, titles, axis labels, etc, later
  })
  
  output$S1Stats <- renderDataTable ({
    Dstats <- data.frame()
    VarList <- c('WIC', 'ATX', 'DPXC', 'PSXC', 'GGALT', 'PALT')
    Ds <- getNetCDF ('/Data/DEEPWAVE/DEEPWAVErf20.nc', VarList)
    ## FL400 means pressure altitude of 40000 ft
    Ds <- Ds[Ds$PALT/0.3048 > 40000, ]  ## select only points above 40000 ft
    save (Ds, file='~cooperw/RStudio/RSessions/RSessions/Session1/Data3.Rdata')
    load ('~cooperw/RStudio/RSessions/RSessions/Session1/Data3.Rdata')
    Dstats['Time', 1] <- 'Time'
    Dstats['Time', 2] <- NA
    Dstats['Time', 3] <- NA
    Dstats['Time', 4] <- formatTime (Ds$Time[1])
    Dstats['Time', 5] <- formatTime (Ds$Time[nrow(Ds)])
    for (nm in names(Ds)) {
      if (nm == 'Time') {next}
      Dstats[nm, 1] <- nm
      Dstats[nm, 2] <- mean (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 3]   <- sd   (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 4]  <- min  (Ds[, nm], na.rm=TRUE)
      Dstats[nm, 5]  <- max  (Ds[, nm], na.rm=TRUE)
    }
    names(Dstats) <- c('variable', 'mean', 'sd', 'min', 'max')
    row.names (Dstats) <- names(Ds)
    for (k in 2:5) {
      Dstats[2:nrow(Dstats), k] <- sprintf('%.3f', as.numeric(Dstats[2:nrow(Dstats), k]))
    }
    Dstats
  })
  
  output$txtCalc1 <- renderUI({
    y <- NA
    {options("digits"=5)
      e <- paste ('y <- round(', input$cformula, ', 6)', sep='')
      try(eval (parse (text=e)), silent=TRUE)
      if (!is.na(y[1])) {
        pre(HTML(y))
      }}
  })
  
  output$txtS2a <- renderUI({
    RT <- input$selS2a
    if (length(RT) == 0 || RT == 1) {
      tx <- paste('Answer will appear here when you select a button below',
                  ' ', ' ', ' ', ' ', ' ', ' ', sep='<br/>')
    }
    a <- 1:12
    if (RT == 2) {tx <- paste('1 2 3 4 5 6 7 8 9 10 11 12', 
                              ' ', 
                              '## the colon operator generates a sequence ', 
                              ' ', ' ', ' ', ' ', sep='<br/>')}
    dim(a) <- c(3,4)
    if (RT == 3) {
      tx <- paste(
        '     [,1] [,2] [,3] [,4] ',
        '[1,]  1    4    7   10 ',
        '[2,]  2    5    8   11 ',
        '[3,]  3    6    9   12 ', 
        ' ', 
        '## c() generates a vector with elements equal to its arguments',
        '## note the column-major order', sep='<br/>')
    }
    if (RT == 4) {
      tx <- paste(
        '     [,1] [,2] [,3]', 
        '[1,]  1    2    3 ',
        '[2,]  4    5    6 ',
        '[3,]  7    8    9 ',
        '[4,] 10   11   12', 
        ' ',
        '## t() is the transpose operator', sep='<br/>')
    }
    pre(HTML(tx))
  })
  
  output$txtS2c1 <- renderUI({
    RT <- input$selS2c1
    if (length(RT) == 0 || RT == 1) {
      tx <- paste('Answer will appear here when you select a button above.',
                  ' ', ' ', ' ', ' ', ' ', ' ', sep='<br/>')
    }
    ## retrieve data.frame Data:
    load('~cooperw/RStudio/RSessions/RSessions/Session2/DataS2b.Rdata')
    load('~cooperw/RStudio/RSessions/RSessions/Session2/txw.Rdata')
    blkline <- '                           '
    
    # Data$ATX[5]
    if (RT == 2) {tx <- paste(txw[1], txw[2],  
                              ' ', '## 5th row in column ATX',
                              '## "$V" without quotes identifies variable V',
                              ' ', ' ', ' ', ' ',
                              sep='<br/>')}
    # Data[5,2]
    if (RT == 3) {tx <- paste(txw[3], txw[4],
                              ' ', '## ATX is the 2nd column.',
                              '## You can also use multiple indices.',
                              '## (Try Data[5,2:5] or Data[5, c(2,4,6)]',
                              ' ', ' ', ' ',
                              sep='<br/>')}
    # Data[5, ]
    if (RT == 4) {tx <- paste(txw[5], ' ',
                              '## Selects all variables in the 5th row.',
                              '## Defines a single-row data.frame.',
                              '## Do not omit the comma; ',
                              '## Data[5] gives a surprising answer.',
                              '## (try it)', ' ', ' ', sep='<br/>')}
    # Data[5, 'ATX']
    if (RT == 5) {tx <- paste(txw[12], txw[13], ' ',
                              '## The character name of a column also works',
                              '## and you can use a vector of names',
                              '## to select multiple columns',
                              ' ', ' ', ' ', sep='<br/>'
    )}
    # Data$ATX
    if (RT == 6) {tx <- paste(txw[14], txw[15], txw[16], ' ',
                              '## This selects the entire column',
                              '## and returns a vector, not a data.frame.',
                              '## Equivalent to Data[, 2].',
                              ' ', ' ', sep='<br/>')}
    if (RT == 7) {tx <- paste('attach(Data); ATX[5]', txw[2],
                              '## This call makes all the variables in',
                              '## the data.frame available as independent',
                              '## variables. Although it is sometimes very',
                              '## useful, it is dangerous because',
                              '## confusion may arise among same-name',
                              '## variables in different environments.', sep='<br/>'
    )}
    if (RT == 8) {tx <- paste('with(Data, ATX[5]', txw[2],
                              '## This is usually better than "attach"',
                              '## because it isolates the scope to the',
                              '## enclosing ( ). However, assignments in',
                              '## the ( ) must use <<- if you want to',
                              '## use them outside the ( ); try:',
                              '##   with(Data, X <- ATX[5])',
                              '##   print (X)', sep='<br/>' 
    )}
    pre(HTML(tx))
  })
  
  output$RS3apng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3aframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3bpng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3bframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3cpng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3cframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS3dpng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session3/S%02d.png', input$S3dframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
  output$RS4png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session4/S%02d.png', input$S4frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5apng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5aframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5bpng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5bframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS5cpng <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session5/S%02d.png', input$S5cframe),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS6png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session6/S%02d.png', input$S6frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS7png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session7/S%02d.png', input$S7frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS8png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session8/S%02d.png', input$S8frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$RS9png <- renderImage ({
    list(src = sprintf('~cooperw/RStudio/RSessions/RSessions/Session9/S%02d.png', input$S9frame),
         contentType = 'image/png',
         width = 800,
         height = 600,
         alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  output$IS1png <- renderImage ({
    list(src = sprintf('www/S%02d.png', input$I1frame),
      contentType = 'image/png',
      width = 800,
      height = 600,
      alt = "RSessions image goes here")
  }, deleteFile = FALSE)
  
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

