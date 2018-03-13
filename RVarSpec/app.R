#
# This is a Shiny web application showing variance spectra. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Ranadu)
library(bspec)
library(scales)
library(nleqslv)

## assemble a list of projects for which an appropriately named rf01
## exists in the data directory:

PJ <- c('SOCRATES', 'WECAN-TEST', 'ARISTO2017', 'ECLIPSE', 'ORCAS', 'CSET', 'NOREASTER', 'HCRTEST', 'WINTER', 'NOMADSS',
  'DEEPWAVE', 'CONTRAST', 'SPRITE-II', 'MPEX', 'DC3', 'RICO',
  'TORERO', 'HIPPO-5', 'HIPPO-4', 'HIPPO-3', 'HIPPO-2',
  'HIPPO-1','PREDICT', 'START08', 'PACDEX', 'TREX')
for (P in PJ) {
  if (grepl('HIPPO', P)) {
    fn <- sprintf ('%sHIPPO/%srf01.nc', DataDirectory (), P)
  } else {
    fn <- sprintf ('%s%s/%srf01.nc', DataDirectory (), P, P)
    if (!file.exists (fn)) {
      fn <- sub ('\\.nc', '.Rdata', fn)
    }
    if (!file.exists (fn)) {
      fn <- sprintf ('%s%s/%stf01.nc', DataDirectory (), P, P)
    }
    if (!file.exists (fn)) {
      fn <- sub ('\\.nc', '.Rdata', fn)
    }  
  }
  if (!file.exists (fn)) {PJ[PJ==P] <- NA}
}
PJ <- PJ[!is.na(PJ)]
## initial variable list:
Project <- 'SOCRATES'
Flight <- 1
FI <- DataFileInfo (sprintf ('%s%s/%stf%02d.nc', DataDirectory(), Project, Project, Flight),
  LLrange=FALSE)
VarLast <- 'none'
fname.last <- 'none'
Trace <- TRUE
formatTime <- function (time) {
  t <- as.POSIXlt (time, tz='UTC')
  tt <- sprintf ("%d:%02d:%02d", t$hour, t$min, as.integer(t$sec))
  return (tt)
}
transferAttributes <- function (dsub, d) {
  ds <- dsub
  ## ds and dsub are the new variables; 
  ## d is the original with attributes
  for (nm in names (ds)) {
    if ((nm != 'Time') && exists ('specialData') &&
        (nm %in% names (specialData))) {next}
    var <- sprintf ("d$%s", nm)
    A <- attributes (eval (parse (text=var)))
    if (!grepl ('Time', nm)) {
      A$dim[1] <- nrow(ds)
      A$class <- NULL
    } else {
      A$dim <- nrow (ds)
    }
    # print (sprintf ('tA: nm=%s, A=%s', nm, A))
    attributes (ds[,nm]) <- A
  }
  A <- attributes (d)
  A$Dimensions$Time$len <- nrow (ds)
  A$row.names <- 1:nrow (ds)
  A$names <- names (ds)
  attributes (ds) <- A
  return(ds)
}



ui <- fluidPage(theme='www/bootstrap.css',
  tags$script(HTML("$(function() {
                                     $(document).keyup(function(e) {
                                       if (e.which == 115) {  ## F4 key
                                         $('#resetT').click()
                                       }
                                     });
                                   })")),  
  # Application title
  titlePanel("Variance Spectra"),
  tags$script(' var setInitialCodePosition = function() { setCodePosition(false, false); }; '),
  
  tabsetPanel (id='whichTab', type='pills',
    tabPanel ('Overview',
      includeHTML('HTML/SpecAnalysisA.html')
    ),
    tabPanel ('Comments on the Distribution Functions',
      includeHTML('HTML/SpecAnalysisB.html')
    ),
    tabPanel ('Interpreting the Variance Spectrum',
      h4('The spectral variance can be regarded as a continuous function, but in practice there are limitations to its determination. This section discusses some of those limitations, notably the Nyquist frequency, and then presents some examples with interpretation.'),
      tabsetPanel (id='subTabBC', type='pills',
        tabPanel ('Some limitations',
          includeHTML ('HTML/SpecAnalysisBC1.html')
        ),
        tabPanel ('Introductory Examples',
          includeHTML ('HTML/SpecAnalysisBC2.html')
        ),
        tabPanel ('Autocorrelation functions',
          includeHTML ('HTML/SpecAnalysisBC3.html')
        ),
        tabPanel ('Variance Spectra',
          includeHTML ('HTML/SpecAnalysisBC4.html')
        ),
        tabPanel ('Effects of Random Error',
          includeHTML ('HTML/SpecAnalysisBC5.html')
        )
      )
    ),
    tabPanel ('Generating Plots',
      h4('It is beyond the scope of this note to describe the detailed construction of the variance spectrum, although a document at this linked URL provides such details. Some of that material is presented in the next sub-section below, but those wanting to skip directly to instructions on how to use “R” to generate the variance spectrum can skip to the following section.'),
      tabsetPanel (id='subTab', type='pills',
        tabPanel ('The periodogram',
          includeHTML ('HTML/SpecAnalysisC1.html')
        ),
        tabPanel ('Generating the Periodogram',
          includeHTML ('HTML/SpecAnalysisC2.html')
        ),
        tabPanel ('Reducing Uncertainty',
          includeHTML ('HTML/SpecAnalysisC3.html')
        ),
        tabPanel ('The Maximum-Entropy Method',
          includeHTML ('HTML/SpecAnalysisC4.html')
        )
      )
    ),
    tabPanel ('Interactive',
      sidebarLayout(
        sidebarPanel(
          fluidRow(
            column(3, selectInput ('project', 'Project', choices=PJ)),
            column(3, numericInput('flight', 'Flight', min=1, max=100, value=1)),
            column(3, radioButtons('typeFlight', 'Type', choices=c('rf', 'tf', 'ff'))),
            column(3, radioButtons('suffix', 'Suffix', choices=c('none', 'HRT', 'Y', 'Z')))
          ), 
          fluidRow(
            column(10, sliderInput("times", label=NA, min=FI$Start, max=FI$End,
              value=c(FI$Start, FI$End),
              # animate=TRUE,
              step=60,
              timeFormat='%T', dragRange=TRUE,
              timezone='+0000')),
            column (2, shinyBS::bsButton ('resetT', label='reset', size='extra-small'))
          ),
          fluidRow(
            column(4, selectInput ('variable', 'Variable', choices=sort(FI$Variables), selected='WIC')),
            column(3, checkboxInput('errors', 'show uncertainty', value=FALSE)),
            column(5, radioButtons ('pvar', 'plot type', choices=c('data', 'fP(f)', 'P(f)', 'autocorrelation'), inline=TRUE))
          ),
          fluidRow(
            column(4, checkboxInput ('method1', 'spectrum', value=TRUE)),
            column(4, numericInput ('span', 'span', min=0, max=199, value=25)),
            column(4, numericInput ('sbins1', 'smooth bins', min=0, max=200, value=0))),
          fluidRow(
            column(4, checkboxInput ('method2', 'welchPSD', value=FALSE)),
            column(4, numericInput ('segl', 'seg l', min=64, max=2048, value=512)),
            column(4, numericInput ('sbins2', 'smooth bins', min=0, max=200, value=0))),
          fluidRow(
            column(4, checkboxInput ('method3', 'MEM', value=FALSE)),
            column(4, numericInput ('poles', 'poles', min=10, max=200, value=50)),
            column(4, numericInput ('sbins3', 'smooth bins', min=0, max=200, value=0))),
          fluidRow(
            column(8, sliderInput('xrange', 'log x-range', min=-4, max=log10(25), value=c(-3, 0))),
            column(4, numericInput ('resl', 'MEM resolution', min=-6, max=-1, value=-3))
          ),
          numericInput ('Gaussian', 'Gaussian noise (+ or -)', min=-10, max=10, value=0)
        ),
        
        mainPanel(
          plotOutput(outputId="specPlot", click=clickOpts(id='plot_click'),
            brush=brushOpts(id='plot_brush', delay=3000, delayType='debounce', resetOnNew=TRUE),
            height='600px', width='70%')
        )
      ),
      includeHTML('HTML/SpecAnalysisD.html')
    ),
    tabPanel ('Appendix: Uncertainty',
      includeHTML ('HTML/SpecAnalysisE.html')
    ),
    tabPanel ('credits',
      includeHTML ('HTML/SpecAnalysisF.html')
    )
  )
)

server <- function(input, output, session) {
  
  reac <- reactiveValues (newdata=0, newdisplay=0)
  ##################################################
  
  data <- reactive({                     ## data
    Project <<- input$project
    Flight <<- input$flight
    TypeFlight <<- input$typeFlight
    Suffix <<- input$suffix
    input$variable
    if (grepl ('HIPPO', Project)) {
      if (Suffix == 'F') {
        fname <<- sprintf ('%sHIPPO/%s%s%02dF.nc', DataDirectory (), Project,
          TypeFlight, Flight)
      } else {
        fname <<- sprintf ('%sHIPPO/%s%s%02d.nc', DataDirectory (), Project,
          TypeFlight, Flight)
      }    
    } else {
      if (Suffix == 'Y') {
        fname <<- sprintf ('%s%s/%s%s%02dY.nc', DataDirectory (), Project,
          Project, TypeFlight, Flight)
      } else if (Suffix == 'HRT') {
        fname <<- sprintf ('%s%s/%s%s%02dHRT.nc', DataDirectory (), Project,
          Project, TypeFlight, Flight)
        if (!file.exists(fname)) {
          fname <<- sprintf ('%s%s/%s%s%02dH.nc', DataDirectory (), Project,
            Project, TypeFlight, Flight)
        }
        if (Trace) {print (sprintf ('in data, file name is %s', fname))}
      } else if (Suffix == 'KF') {
        fname <<- sprintf ('%s%s/%s%s%02dKF.nc', DataDirectory (), Project,
          Project, TypeFlight, Flight)
      } else {
        fname <<- sprintf ('%s%s/%s%s%02d.nc', DataDirectory (), Project,
          Project, TypeFlight, Flight)
      }
    }
    #     if (input$Production) {
    #       print (sprintf ('Production section, input$Production=%d', input$Production))
    #       dr <- sprintf ('%s../raf/Prod_Data/%s', DataDirectory (), Project)
    #       scmd <- sprintf ('ls -lt `/bin/find %s -ipath "\\./movies" -prune -o -ipath "\\./*image*" -prune -o -name %s%s%02d.nc`',
    #                        dr, Project, input$typeFlight, input$Flight)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #       scmd <- sub ('\\.nc', '.Rdata', scmd)
    #       fl <- system (scmd, intern=TRUE)[1]
    #       if ((length (fl) > 0) && (!grepl ('total', fl))) {
    #         fname <- sub ('.* /', '/', fl[1])
    #       }
    #     }
    print (sprintf ('data: fname=%s', fname))
    if (file.exists(fname)) {
      FI <<- DataFileInfo (fname, LLrange=FALSE)
      ## reset xrange to remain below the Nyquist frequency
      if (FI$Rate/2 < isolate(10^input$xrange[2])) {
        updateSliderInput(session, 'xrange', value=c(-3,log10(FI$Rate/2)))
      }
      VarList <<- c('TASX', input$variable)
      if ((fname != fname.last) || (VarList != VarLast)) {
        if (Trace) {print (sprintf ('reading data from %s; VarList is %s', fname, VarList))}
        D <- getNetCDF (fname, VarList)
        if ('GGVSPDB' %in% VarList) {
          D$GGVSPD <- D$GGVSPDB
        } else if ('VSPD_A' %in% VarList) {
          D$GGVSPD <- D$VSPD_A
        } else if ('VSPD_G' %in% VarList) {
          D$GGVSPD <- D$VSPD_G
        }
        ## beware of cases with a long string of NAs at the start of the flight
        if ('TASX' %in% names (D)) {
          ix <- which (!is.na(D$TASX))
          TatStart <- D$Time[ix[1]]
          TatEnd <- D$Time[ix[length(ix)]]
          DS <- D
          D <- D[D$Time >= TatStart & D$Time <= TatEnd, ]
          D <- transferAttributes (D, DS)
          rm (DS)
        }
        if (fname != fname.last) {
          # plotSpec$Times <<- c(D$Time[1], D$Time[nrow(D)])
          step <- 60
          minT <- D$Time[1]
          minT <<- minT <- minT - as.integer (minT) %% step + step
          maxT <- D$Time[nrow(D)]
          maxT <<- maxT <- maxT - as.integer (maxT) %% step
          # plotSpec$Times <<- c(D$Time[1], D$Time[nrow(D)])
          Times <<- c(minT, maxT)
          if (Trace) {print (sprintf ('data: setting plotSpec$Times to %s %s', 
            formatTime (minT), formatTime (maxT)))}
          updateSliderInput (session, 'times', value=Times, min=minT, max=maxT)
          updateSelectInput (session, 'variable', choices=sort(FI$Variables), selected='WIC')
        }
        if (Trace) {print (sprintf ('data: loaded data.frame from %s', fname))}
        Data <<- D
      } else {  ## fname and variables are the same, so reuse Data
        D <- Data
      }
      
      if (length (D) > 1) {
        fname.last <<- fname
        # VarLast <<- VarList
        # Data <<- D
        Rate <<- FI$Rate
        times <- input$times
        Times <- as.integer (gsub (':','', formatTime(times)))
        print (Times)
        D <- Data[setRange(Data, Times[1], Times[2]), ]
        # isolate(reac$newdisplay <- reac$newdisplay + 1)
        return (D)
      } else {
        print (sprintf ('fname=%s', fname))
        print (VarList)
        ## stopping to prevent looping
        stop ('variable not found; stopping to avoid looping')
      }
    } else {
      warning (sprintf ('the file %s does not exist', fname))
      fnRdata <- sub ('\\.nc', '.Rdata', fname)
      if (file.exists (fnRdata)) {
        warning ('using Rdata file instead')
        fl <- load (file=fnRdata)
        FI <<- DataFileInfo (fnRdata)
        # loadVRPlot (Project, Production=FALSE, input$Flight, psq)
        fname.last <<- fname
        # print (sprintf ('data returned with dimensions %d', dim(Data)))
        return (Data)
      }
    }
  })
  ##################################################
  
  output$specPlot <- renderPlot({
    # reac$newdisplay
    # print(input$method)
    # Project <- input$project
    # if (input$suffix == 'none') {
    #   fname <- sprintf('%s%s/%s%s%02d.nc', DataDirectory(), Project, Project,
    #   input$typeFlight, input$flight)
    #   Data <- getNetCDF(fname, input$variable)
    # } else if (input$suffix == 'HRT') {
    #   fname <- sprintf('%s%s/%s%s%02dH.nc', DataDirectory(), Project, Project,
    #     input$typeFlight, input$flight)
    #   if (!file.exists (fname)) {
    #     fname <- sprintf('%s%s/%s%s%02dHRT.nc', DataDirectory(), Project, Project,
    #       input$typeFlight, input$flight)
    #   }
    #   Data <- getNetCDF(fname, input$variable)
    # }
   
    Data <- data()
    print ('names returned from data():')
    print (names (Data))
    if (input$pvar == 'data') {
      plotWAC(Data$Time, Data[, 3])
      plots <- 0
    } else if (input$pvar == 'autocorrelation') {
      sink("/dev/null");v <- detrend(Data[, c('Time', input$variable)]);sink()
      if (input$Gaussian != 0) {
        v <- v + rnorm (length(v), 0, 1) * input$Gaussian
      }
      AC <- acf (ts(v, frequency=Rate), lag.max=10, plot=FALSE)
      AC <<- AC
      g <- ggplotWAC(data.frame(AC$lag, AC$acf)) + xlab('lag [s]') + ylab('ACF')
      ## try extrapolating and exponential to lag-0:
      taufit <- mean (AC$acf[3:6]/AC$acf[2:5])
      ## find the best-fit intercept and time constant
      chisqr <- function (tau) {
        A1 <- sum(AC$acf[2:5]*c(2:5)*exp(-c(2:5)/tau))
        A2 <- sum(c(2:5)*exp(-2*c(2:5)/tau))
        A3 <- sum(AC$acf[2:5]*exp(-c(2:5)/tau))
        A4 <- sum(exp(-2*c(2:5)/tau))
        ch <- abs(A1/A2-A3/A4)
        return(ch)
      }
      taufit <- nleqslv(10, chisqr)
      tau <- taufit$x
      ac0 <- sum(AC$acf[2:5]*exp(-c(2:5)/tau)) / sum(exp(-2*c(2:5)/tau))
      if (ac0 > 1.01) {ac0 <- 1.01}
      # print (sprintf ('ac0=%f, taufit=%f', ac0, taufit$x))
      AC0 <- data.frame(x=0, y=ac0)
      g <- g + geom_point(data=AC0, aes(x=x, y=y), colour='red', na.rm=TRUE)
      plots <- 1
    } else {FI <- DataFileInfo (fname)

    if (!('GGVSPD' %in% FI$Variables)) {
      if ('GGVSPDB' %in% FI$Variables) {
        VarList [which (VarList == 'GGVSPD')] <- 'GGVSPDB'
      } else if ('VSPD_A' %in% FI$Variables) {
        VarList [which (VarList == 'GGVSPD')] <- 'VSPD_A'
      } else if ('VSPD_G' %in% FI$Variables) {
        VarList [which (VarList == 'GGVSPD')] <- 'VSPD_G'
      } else {
        print ('ERROR: no VSPD variable found')
        exit()
      }
    }
    for (Var in VarList) {
      if (!(Var %in% FI$Variables)) {
        print (sprintf (' required variable %s not found in file %s; skipping...', Var, fname))
        exit()
      }
    }
    
      if (length(input$method)) {
        plot(c(0,1),c(0,1), axes=FALSE, col='white', xlab='', ylab='')
        text(0.5,0.5,'select a method')
      }
      plots <- 0
      siglim <- 1  ## one-standard-error estimates
      ylim=c(0.01,1000)
      if (input$pvar == 'P(f)') {ylim <- c(1.e-4,1.e4)}
      # xlim=c(0.001,log10(25))
      xlim <- 10^input$xrange
      if (abs(xlim[2]-12.5) < 0.01) {xlim[2] <- 16}
      # v <- DIA[, input$variable]
      # sink("/dev/null");v <- detrend(Data[, c('Time', input$variable)]);sink()
      print (input$variable)
      print (names(Data))
      v <- detrend(Data[, c('Time', input$variable)])
      # v <- v + input$Gaussian * rnorm(length(v), 0, 1)
      if (input$method1) {
        spans <- (input$span %/% 2) * 2 + 1
        if (spans < 3) {
          S <- spectrum (ts(v, frequency=Rate), plot=FALSE)
        } else {
          S <- spectrum (ts(v, frequency=Rate), span=spans, plot=FALSE)
        }
        S <<- S
        coverage <- pnorm(1)-pnorm(-1)  ## 1-sigma, 0.68269
        tail <- 1 - coverage
        df <- S$df
        uq <- 1 - tail * pchisq(df, df, lower.tail = FALSE)
        lq <- tail * pchisq(df, df, lower.tail=TRUE)
        ci <- 1/(qchisq(c(uq, lq), df)/df)
        lower.limit <- qchisq (pnorm(-siglim), df) / df
        upper.limit <- qchisq (pnorm(siglim), df) / df
        # print (sprintf ('ci=%.3f -- %.3f', ci[1], ci[2]))
        freq <- S$freq
        fpf <- 2 * S$spec + sign(input$Gaussian) * input$Gaussian^2 / 12  ## note factor of 2, required for one-sided normalization
        if (input$pvar == 'fP(f)') {fpf <- fpf * freq}
        if(input$sbins1 > 0) {
          bs1 <- binStats(data.frame(fpf, log(freq)), bins=input$sbins1)
          bs1 <- rbind (bs1, data.frame(xc=bs1$xc[nrow(bs1)], ybar=bs1$ybar[nrow(bs1)], 
            sigma=bs1$sigma[nrow(bs1)], nb=1)) 
          freq <- exp(bs1$xc)
          fpf <- bs1$ybar
          bs1$sigma <- ifelse (bs1$nb > 2, bs1$sigma/sqrt(bs1$nb), NA)
          rna <- is.na(bs1$sigma)
          bs1$sigma[rna] <- bs1$ybar[rna] / 2
          bs1 <<- bs1
        }
        # plotWAC(freq, fpf, xlab='frequency', log='xy', ylim=ylim, xlim=xlim)
        fpfmin <- (floor  (min(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        fpfmax <- (ceiling(max(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        # print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
        # print (summary(fpf))
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        fpfmax <- 10^fpfmax
        fpfmin <- 10^fpfmin
        ylim=c(fpfmin, fpfmax)
        cLines <- 'blue'
        names(cLines) <- 'spectrum'
        lfpf <- length(fpf)
        y1 <- ci[1]*fpf[lfpf]
        y2 <- ci[2]*fpf[lfpf]
        y1 <- lower.limit * fpf[lfpf]
        y2 <- upper.limit * fpf[lfpf]
        DL <- data.frame(x4=13, y4=fpf[lfpf], y4min=y1, y4max=y2)
        labx <- 'frequency [Hz]'
        laby <- sprintf('%s for %s', input$pvar, input$variable)
        g <- ggplot(data=data.frame(freq, fpf))
        g <- g + geom_path (aes(x=freq, y=fpf, colour='spectrum'), na.rm=TRUE) + 
          xlab(labx) + ylab (laby) 
        tasAverage <- mean(Data$TASX, na.rm=TRUE)
        ae <- ifelse (input$variable %in% c('TASX', 'UXC', 'UXG'), 0.15, 0.2)
        for (i in (-8:0)) {
          a = ae * 10.^(i*(2/3)) * tasAverage^(2/3)
          lw = ifelse(i == -4, 1.2, 0.5)
          DFL <- data.frame(x=xlim, y=c(a/xlim[1]^(2/3), a/xlim[2]^(2/3)))
          print(DFL)
          g <- g + geom_path (data=DFL, aes(x=x, y=y), colour='darkorange', lwd=lw, lty=3)
        }
        if (input$pvar == 'fP(f)') {
          line53 <- data.frame(x=c(1.e-3, 1), y=c(1, 1.e-2))  
        } else {
          line53 <- data.frame(x=c(1.e-3, 1), y=c(1, 1.e-5))
        }
        # g <- g + geom_path (data=line53, aes(x=x, y=y), colour='darkorange')
        if (input$errors) {
          g <- g + geom_errorbar(aes(x=x4, ymin=y4min, ymax=y4max), data=DL, width=0.05, 
            size=1, colour='blue', inherit.aes=FALSE, na.rm=TRUE) +
            geom_point(data=DL, aes(x=x4, y=y4), pch=20, colour='blue', size=4, na.rm=TRUE)
        }
        # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
        # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
        # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
        g <- g + (scale_colour_manual (name='', values=cLines)) +
          scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), #limits = xlim,
            labels = trans_format("log10", math_format(10^.x))) +
          scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=5), #limits = ylim,
            labels = trans_format("log10", math_format(10^.x))) +
          coord_cartesian(xlim=xlim, ylim=ylim) +
          theme_WAC()
        if (input$sbins1 > 10 && input$errors) {
          g <- g + geom_ribbon(data=bs1, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
            fill='cyan', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
        }
        # lines(freq, fpf*ci[2], col='black', lwd=1, lty=2)
        # lines(freq, fpf*ci[1], col='black', lwd=1, lty=2)
        plots <- 1
      } 
      if (input$method2) {
        ## force segl to a power of 2
        segl <- input$segl
        rsl <- log(segl) / log(2)
        ns <- round (rsl)
        if (2^ns != segl) {
          if (2^ns > segl) {segl <- 2^(ns-1)}
          else {segl <- 2^(ns+1)}
          updateNumericInput(session, 'segl', value=segl)
          return()
        }
        BSP <- bspec::bspec(ts(v, frequency=Rate))
        S2 <- bspec::welchPSD (ts(v, frequency=Rate), seglength=input$segl, 
          windowfun=bspec::hammingwindow)
        S2 <<- S2
        # ci <- quantile.bspec(BSP, probs = c(0.025, 0.975),
        #   two.sided = FALSE)
        coverage <- 0.683
        tail <- 1 - coverage
        df <- 2 * 9 * S2$segments / 11 ##1.768849
        upper.quantile <- 1 - tail * pchisq(df, df, lower.tail = FALSE) 
        lower.quantile <- tail * pchisq(df, df)
        ci <- 1/(qchisq(c(upper.quantile, lower.quantile), df)/df) 
        df <- 1.46 * (S2$segments + 1)
        lower.limit <- qchisq (pnorm(-siglim), df) / df
        upper.limit <- qchisq (pnorm(siglim), df) / df
        # ci <- 0.5 + (ci-0.5) / sqrt(9 * S2$segments / 11)
        # print (sprintf ('ci2=%.3f -- %.3f segments %d', ci[1], ci[2], S2$segments))
        freq <- S2$frequency[-1]
        fpf <- S2$power[-1]
        if (input$pvar == 'fP(f)') {fpf <- fpf * freq}
        if(input$sbins2 > 0) {
          bs2 <- binStats(data.frame(fpf, log(freq)), bins=input$sbins2)
          bs2 <- rbind (bs2, data.frame(xc=bs2$xc[nrow(bs2)], ybar=bs2$ybar[nrow(bs2)], 
            sigma=bs2$sigma[nrow(bs2)], nb=1))
          freq <- exp(bs2$xc)
          fpf <- bs2$ybar
          bs2$sigma <- ifelse (bs2$nb > 2, bs2$sigma/sqrt(bs2$nb), NA)
          rna <- is.na(bs2$sigma)
          bs2$sigma[rna] <- bs2$ybar[rna] / 2
        }
        fpfmin <- (floor  (min(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        fpfmax <- (ceiling(max(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        # print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
        # print (summary(fpf))
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        fpfmax <- 10^fpfmax
        fpfmin <- 10^fpfmin
        ylim=c(fpfmin, fpfmax)
        lfpf <- length(fpf)
        y1 <- ci[1]*fpf[lfpf]
        y2 <- ci[2]*fpf[lfpf]
        y1 <- lower.limit * fpf[lfpf]
        y2 <- upper.limit * fpf[lfpf]
        DL5 <- data.frame(x5=14.2, y5=fpf[lfpf], y5min=y1, y5max=y2)
        if (plots == 0) {
          # plotWAC(freq, fpf, xlab='frequency', log='xy', ylim=ylim, xlim=xlim, col='forestgreen')
          cLines <- 'forestgreen'
          names(cLines) <- 'Welch'
          labx <- 'frequency [Hz]'
          laby <- sprintf('%s for %s', input$pvar, input$variable)
          g <- ggplot(data=data.frame(freq, fpf))
          g <- g + geom_path (aes(x=freq, y=fpf, colour='Welch'), na.rm=TRUE) +
            xlab(labx) + ylab (laby)
          if (input$errors) {
            g <- g + geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
              size=1, colour='forestgreen', inherit.aes=FALSE, na.rm=TRUE) +
              geom_point(data=DL5, aes(x=x5, y=y5), pch=20, colour='forestgreen', 
                size=4, inherit.aes=FALSE, na.rm=TRUE)
          }
          # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
          # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
          # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
          g <- g + (scale_colour_manual (name='', values=cLines)) +
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), limits = xlim,
              labels = trans_format("log10", math_format(10^.x))) +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=5), limits = ylim,
              labels = trans_format("log10", math_format(10^.x))) +
            theme_WAC() 
          if (input$sbins2 > 10 && input$errors) {
            g <- g + geom_ribbon(data=bs2, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
              fill='green', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
          }
          plots <- 1
        } else {
          ncl <- names(cLines)
          cLines <- c(cLines, 'forestgreen')
          names(cLines) <- c(ncl, 'Welch')
          DFB <- data.frame(freq, fpf)
          g <- g + geom_path (aes(x=freq, y=fpf, colour='Welch'), data=DFB, na.rm=TRUE)
          if (input$errors) {
            g <- g + geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
              size=1, colour='forestgreen', inherit.aes=FALSE, na.rm=TRUE) +
              geom_point(data=DL5, aes(x=x5, y=y5), pch=20, colour='forestgreen', 
                size=4, inherit.aes=FALSE, na.rm=TRUE)
          }
          g <- g + (scale_colour_manual (name='', values=cLines))
          if (input$sbins2 > 10 && input$errors) {
            g <- g + geom_ribbon(data=bs2, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
              fill='green', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
          }
        }
        S2 <<- S2
        # lines (c(13,13), ci, lwd=3)
        # lfpf <- length(fpf)
        # lines(c(14,14), (ci)*fpf[lfpf], lwd=2)
        # lines(c(13.7,14.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
        # lines(c(13.7,14.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
        # lines(BSP$freq, ci[, 1], col='forestgreen', lwd=0.5)
        # lines(BSP$freq, ci[, 2], col='forestgreen', lwd=0.5)
      }
      if (input$method3) {
        MEMc <- memCoef (v, .poles=input$poles)
        lf <- seq(input$xrange[1], input$xrange[2], by=10^input$resl)
        freq <- 10^lf
        Rate <- FI$Rate
        Pmem <- memEstimate (freq/Rate, MEMc) / Rate
        Pmem <- 2 * Rate * Mod(Pmem)^2         ## The returned spectrum is complex
        fpf <- Pmem
        if (input$pvar == 'fP(f)') {fpf <- fpf * freq}
        lfpf <- length(fpf)
        sigm <- length(v) - input$poles - 1/10^input$resl - 1
        sigm <- ifelse (sigm > 0, sqrt(sigm), 1.1)
        y1 <- fpf[lfpf] * (1-1/sigm)
        y2 <- fpf[lfpf] * (1+1/sigm)
        df <- length(v) / input$poles
        lower.limit <- qchisq (pnorm(-siglim), df) / df
        upper.limit <- qchisq (pnorm(siglim), df) / df
        y1 <- lower.limit * fpf[lfpf]
        y2 <- upper.limit * fpf[lfpf]
        DL6 <- data.frame(x6=15, y6=fpf[lfpf], y6min=y1, y6max=y2)
        DL7 <- data.frame(y7=fpf[lfpf]*50, x7min=freq[lfpf]*(10^(-10^input$resl)), x7max=freq[lfpf])
        # print (c('DL6', DL6))
        # print (c('DL7', DL7))
        if(input$sbins3 > 0) {
          bs3 <- binStats(data.frame(fpf, log(freq)), bins=input$sbins3)
          bs3 <- rbind (bs3, data.frame(xc=bs3$xc[nrow(bs3)], ybar=bs3$ybar[nrow(bs3)], 
            sigma=bs3$sigma[nrow(bs3)], nb=1)) 
          freq <- exp(bs3$xc)
          fpf <- bs3$ybar
          bs3$sigma <- ifelse (bs3$nb > 2, bs3$sigma/sqrt(bs3$nb), NA)
          # if (is.na(bs3$sigma)) {bs3$sigma <- bs3$ybar}
        }
        fpfmin <- (floor  (min(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        fpfmax <- (ceiling(max(log10(fpf[freq >= 10^input$xrange[1] & freq <= 10^input$xrange[2]]), na.rm=TRUE)))
        # print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
        # print (summary(fpf))
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        if (fpfmax - fpfmin < 5) {fpfmin <- fpfmin - 1}
        if (fpfmax - fpfmin < 5) {fpfmax <- fpfmax + 1}
        fpfmax <- 10^fpfmax
        fpfmin <- 10^fpfmin
        ylim=c(fpfmin, fpfmax)
        if (plots == 0) {
          # plotWAC(freq, fpf, log='xy', col='red', lwd=2,
          # xlab='frequency [/day]', ylab='fP(f)', ylim=ylim, xlim=xlim)
          cLines <- 'red'
          names(cLines) <- 'MEM'
          # DL1 <- data.frame(x1=c(15,15), y1=c(y1,y2))
          # print(DL1)
          # DL2 <- data.frame(x2=c(13.7, 14.3), y2=c(y1,y1))
          # DL3 <- data.frame(x3=c(13.7, 14.3), y3=c(y2,y2))
          labx <- 'frequency [Hz]'
          laby <- sprintf('%s for %s', input$pvar, input$variable)
          g <- ggplot(data=data.frame(freq, fpf))
          g <- g + geom_path (aes(x=freq, y=fpf, colour='MEM'), na.rm=TRUE) +
            xlab(labx) + ylab (laby)
          if (input$errors) {
            g <- g + geom_errorbar(aes(x=x6, ymin=y6min, ymax=y6max), data=DL6, width=0.04, 
              size=1, colour='red', inherit.aes=FALSE, na.rm=TRUE) +
              geom_errorbarh(aes(y=y7, xmin=x7min, xmax=x7max), data=DL7, colour='red',
                height=0.4, inherit.aes=FALSE) +
              geom_point(data=DL6, aes(x=x6, y=y6), pch=20, colour='red', size=4, na.rm=TRUE)
          }
          # geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
          #   size=1, colour='forestgreen', inherit.aes=FALSE) +
          # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
          # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
          # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
          g <- g + (scale_colour_manual (name='', values=cLines)) +
            scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), limits = xlim,
              labels = trans_format("log10", math_format(10^.x))) +
            scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=5), limits = ylim,
              labels = trans_format("log10", math_format(10^.x))) +
            theme_WAC()
          if (input$sbins3 > 10 && input$errors) {
            g <- g + geom_ribbon(data=bs3, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
              fill='red', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
          }
          plots <- 1
        } else {
          # lines (freq, fpf, col='red', lwd=2)
          ncl <- names(cLines)
          cLines <- c(cLines, 'red')
          names(cLines) <- c(ncl, 'MEM')
          DFC <- data.frame(freq, fpf)
          g <- g + geom_path (aes(x=freq, y=fpf, colour='MEM'), data=DFC, na.rm=TRUE)
          if (input$errors) {
            # geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
            #   size=1, colour='forestgreen', inherit.aes=FALSE) + 
            g <- g + geom_errorbar(aes(x=x6, ymin=y6min, ymax=y6max), data=DL6, width=0.04, 
              size=1, colour='red', inherit.aes=FALSE, na.rm=TRUE) +
              geom_errorbarh(aes(y=y7, xmin=x7min, xmax=x7max), data=DL7, colour='red',
                height=0.4, inherit.aes=FALSE, na.rm=TRUE) +
              geom_point(data=DL6, aes(x=x6, y=y6), pch=20, colour='red', size=4, na.rm=TRUE)
          }
          g <- g + (scale_colour_manual (name='', values=cLines))
          if (input$sbins3 > 10 && input$errors) {
            g <- g + geom_ribbon(data=bs3, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
              fill='red', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
          }
        }
        # points (freq, Pmem*freq, pch=20, cex=0.5, col='red')
      }
    }
    if (exists ('g')) {g <<- g}
    if (plots) {
      Tm <- c(Data$Time[1], Data$Time[nrow(Data)])
      if (Suffix == 'none') {
        g <- g + ggtitle(sprintf('%s flight %s%02d %s-%s', Project, TypeFlight, Flight,
          formatTime(Tm[1]), formatTime(Tm[2])))
      } else {
        g <- g + ggtitle(sprintf('%s flight %s%02d%s %s-%s', Project, TypeFlight, Flight,
          Suffix, formatTime(Tm[1]), formatTime(Tm[2])))
      }
      print(g)
    }
  })
  
  
  
  observeEvent (input$resetT, {
    print (sprintf ('reached resetT'))
    print (sprintf ('Data times are %s %s', Data$Time[1], Data$Time[nrow(Data)]))
    step <- 60
    minT <- Data$Time[1]
    minT <- minT - as.integer (minT) %% step + step
    maxT <- Data$Time[nrow(Data)]
    maxT <- maxT - as.integer (maxT) %% step
    times <- c(minT, maxT)
    print (sprintf ('resetting times to %s %s', times[1], times[2]))
    updateSliderInput (session, 'times', value=times, min=minT, max=maxT)
  } )
  
  observeEvent (input$plot_click, {
    print (input$plot_click)
    xcursor <- as.integer(input$plot_click$x)
    xcursor <- xcursor - xcursor %% 60  ## set even minute
    ycursor <- input$plot_click$y
    T1 <- as.POSIXlt(xcursor, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    checkTime <<- T1  ## selected to be even-minute value
    updateTextInput (session, 'RefT', value=formatTime (checkTime))
    print (sprintf ('click position is %d %f', TB1, ycursor))
  } )
  
  observeEvent (input$plot_brush, {
    xmin <- as.integer(input$plot_brush$xmin)
    xmax <- as.integer(input$plot_brush$xmax)
    print (sprintf ('xmin/xmax=%d %d', xmin, xmax))
    T1 <- as.POSIXlt(xmin, origin='1970-01-01', tz='UTC')
    T2 <- as.POSIXlt(xmax, origin='1970-01-01', tz='UTC')
    TB1 <- T1$hour*10000 + T1$min*100 + T1$sec
    TB2 <- T2$hour*10000 + T2$min*100 + T2$sec
    print (sprintf ('brush times are %d %d', TB1, TB2))
    updateSliderInput (session, 'times', value=c(T1, T2), min=FI$Start, max=FI$End)
  } )
  
}

# Run the application 
shinyApp(ui = ui, server = server)

