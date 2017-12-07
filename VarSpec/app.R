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

## get the DIA hourly observations, July 1996 to Dec 2005
load(file='DIA.Rdata')
# interpolate for missing values:
for (v in names(DIA)) {
  if (v == 'Time') {next}
  DIA[, v] <- SmoothInterp (DIA[, v], .Length=0)
}
# fix trailing pressure
l <- nrow(DIA)
DIA$SLVP[l] <- DIA$SLVP[l-1]
## add wind components from the north and east 
DIA$WN <- DIA$WS * cos (DIA$WD*pi/180)
DIA$WE <- DIA$WS * sin (DIA$WD*pi/180)
## recover WD using ANG <- atan2(DIA$WE, DIA$WN) * 180 / pi
## save some stuff for now (temporary)
Rate <- 24
# ts1 <- ts(DIA$WN, frequency=24)
# SP <- spectrum(ts1, span=49)
# plotWAC(SP$freq, SP$spec*2*SP$freq, xlab='frequency', log='xy')
# lines(c(10,0.01), c(1,100), col='red')
# hist(DIA$WD, breaks=36, freq=FALSE)
# bs <- Ranadu::binStats(data.frame(ws=DIA$WS, wd=DIA$WD), bins=36)
# Ranadu::plotWAC(bs$xc, bs$ybar, type='s', xlab='WD')
# Ranadu::plotWAC(bs$xc, bs$ybar, type='s', xlab='WD', ylim=c(0,10))
# lines(bs$xc, bs$ybar+bs$sigma, type='s', col='red')
# lines(bs$xc, bs$ybar-bs$sigma, type='s', col='red')
# smoothScatter(DIA$WE, DIA$WN)


ui <- fluidPage(theme='www/bootstrap.css',
  
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
            column(6, selectInput ('variable', 'variable?', 
              choices=c('SLVP', 'ALTP', 'TMCD', 'WS', 'WN','WE'))),  ## WD excluded
            column(6, checkboxInput ('errors', 'show uncertainty', value=FALSE))
          ),
          radioButtons ('pvar', 'plot type', choices=c('fP(f)', 'P(f)', 'autocorrelation'), inline=TRUE),
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
            column(8, sliderInput('xrange', 'log x-range', min=-4, max=log10(12), value=c(-3, log10(12)))),
            column(4, numericInput ('resl', 'MEM resolution', min=-6, max=-1, value=-3))
          ),
          numericInput ('Gaussian', 'Gaussian noise (+ or -)', min=-10, max=10, value=0)
        ),
        
        mainPanel(
          plotOutput("specPlot")
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
  
  output$specPlot <- renderPlot({
    # print(input$method)
    if (input$pvar == 'autocorrelation') {
      v <- detrend(DIA[, c('Time', input$variable)])
      if (input$Gaussian != 0) {
        v <- v + rnorm (length(v), 0, 1) * input$Gaussian
      }
      AC <- acf (ts(v, frequency=Rate), lag.max=10, plot=FALSE)
      AC <<- AC
      g <- ggplotWAC(data.frame(AC$lag, AC$acf)) + xlab('lag [days]') + ylab('ACF')
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
      print (sprintf ('ac0=%f, taufit=%f', ac0, taufit$x))
      AC0 <- data.frame(x=0, y=ac0)
      g <- g + geom_point(data=AC0, aes(x=x, y=y), colour='red')
      plots <- 1
    } else {
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
    if (abs(xlim[2]-12) < 0.01) {xlim[2] <- 16}
    # v <- DIA[, input$variable]
    v <- detrend(DIA[, c('Time', input$variable)])
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
      print (sprintf ('ci=%.3f -- %.3f', ci[1], ci[2]))
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
      print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
      print (summary(fpf))
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
      labx <- expression(paste('frequency [day'^"-1",']', sep=''))
      g <- ggplot(data=data.frame(freq, fpf))
      g <- g + geom_path (aes(x=freq, y=fpf, colour='spectrum')) +
        xlab(labx) + ylab ('f P(f)') 
        if (input$errors) {
          g <- g + geom_errorbar(aes(x=x4, ymin=y4min, ymax=y4max), data=DL, width=0.05, 
                                 size=1, colour='blue', inherit.aes=FALSE) +
                   geom_point(data=DL, aes(x=x4, y=y4), pch=20, colour='blue', size=4)
        }
        # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
        # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
        # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
        g <- g + scale_colour_manual (name='', values=cLines) +
        scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=4), limits = xlim,
          labels = trans_format("log10", math_format(10^.x))) +
        scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x, n=5), limits = ylim,
          labels = trans_format("log10", math_format(10^.x))) +
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
      print (sprintf ('ci2=%.3f -- %.3f segments %d', ci[1], ci[2], S2$segments))
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
      print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
      print (summary(fpf))
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
        labx <- expression(paste('frequency [day'^"-1",']', sep=''))
        g <- ggplot(data=data.frame(freq, fpf))
        g <- g + geom_path (aes(x=freq, y=fpf, colour='Welch')) +
          xlab(labx) + ylab ('f P(f)')
        if (input$errors) {
          g <- g + geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
                                 size=1, colour='forestgreen', inherit.aes=FALSE) +
                   geom_point(data=DL5, aes(x=x5, y=y5), pch=20, colour='forestgreen', 
                     size=4, inherit.aes=FALSE)
        }
          # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
          # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
          # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
          g <- g + scale_colour_manual (name='', values=cLines) +
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
        g <- g + geom_path (aes(x=freq, y=fpf, colour='Welch'), data=DFB)
        if (input$errors) {
          g <- g + geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
                                 size=1, colour='forestgreen', inherit.aes=FALSE) +
                   geom_point(data=DL5, aes(x=x5, y=y5), pch=20, colour='forestgreen', 
                              size=4, inherit.aes=FALSE)
        }
        g <- g + scale_colour_manual (name='', values=cLines)
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
      Rate <- 24
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
      print (c('DL6', DL6))
      print (c('DL7', DL7))
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
      print (sprintf ('fpfmin/fpfmax=%.3f,%.3f', fpfmin, fpfmax))
      print (summary(fpf))
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
        labx <- expression(paste('frequency [day'^"-1",']', sep=''))
        g <- ggplot(data=data.frame(freq, fpf))
        g <- g + geom_path (aes(x=freq, y=fpf, colour='MEM')) +
          xlab(labx) + ylab ('f P(f)')
        if (input$errors) {
          g <- g + geom_errorbar(aes(x=x6, ymin=y6min, ymax=y6max), data=DL6, width=0.04, 
                                 size=1, colour='red', inherit.aes=FALSE) +
                   geom_errorbarh(aes(y=y7, xmin=x7min, xmax=x7max), data=DL7, colour='red',
                                  height=0.4, inherit.aes=FALSE) +
                   geom_point(data=DL6, aes(x=x6, y=y6), pch=20, colour='red', size=4)
        }
          # geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
          #   size=1, colour='forestgreen', inherit.aes=FALSE) +
          # lines(c(13,13), (ci)*fpf[lfpf], lwd=2)
          # lines(c(12.7,13.3), c((ci[1])*fpf[lfpf], (ci[1])*fpf[lfpf]), lwd=2)
          # lines(c(12.7,13.3), c((ci[2])*fpf[lfpf], (ci[2])*fpf[lfpf]), lwd=2)
         g <- g + scale_colour_manual (name='', values=cLines) +
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
        g <- g + geom_path (aes(x=freq, y=fpf, colour='MEM'), data=DFC)
        if (input$errors) {
          # geom_errorbar(aes(x=x5, ymin=y5min, ymax=y5max), data=DL5, width=0.05, 
          #   size=1, colour='forestgreen', inherit.aes=FALSE) + 
          g <- g + geom_errorbar(aes(x=x6, ymin=y6min, ymax=y6max), data=DL6, width=0.04, 
                                 size=1, colour='red', inherit.aes=FALSE) +
                   geom_errorbarh(aes(y=y7, xmin=x7min, xmax=x7max), data=DL7, colour='red',
                                  height=0.4, inherit.aes=FALSE) +
                   geom_point(data=DL6, aes(x=x6, y=y6), pch=20, colour='red', size=4)
        }
        g <- g + scale_colour_manual (name='', values=cLines)
        if (input$sbins3 > 10 && input$errors) {
          g <- g + geom_ribbon(data=bs3, aes(x=exp(xc), ymin=ybar-sigma, ymax=ybar+sigma), 
            fill='red', alpha=0.25, show.legend=FALSE, inherit.aes=FALSE, na.rm=TRUE)
        }
      }
      # points (freq, Pmem*freq, pch=20, cex=0.5, col='red')
    }
    }
    if (plots) {print(g)}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

