#
# This is a Shiny web application that treats geopotential height. 
# You can run the application by clicking the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Ranadu)
library(grid)
library(scales)
library(plotly)
library(nleqslv)
# source('ggplotWAC.R')
# source ('Constants.R')
# source ('MixingRatio.R')
# source ('PotentialTemperatures.R')
# source ('Gravity.R')
# source('PressureAltitude.R')
# source('geoPH.R')
# source('theme_WAC.R')
# source ('LagrangeInterpolation.R')
# source ('MurphyKoop.R')

geomH <- function (geopht, .latitude, .geoid=0) {
  latitude <- .latitude
  geoid <- .geoid
  fng <- function (z) {
    return (GeoPotHeight(latitude, z, geoid) - geopht)
  }
  return(nleqslv(geopht, fng)$x)
}

lp <- seq(0, 3, 0.005)
p <- rev(c(10^lp, 1010))
z <- PressureAltitude(p)
z <- as.integer (z)
zm45 <- geomH(z, 45) * 0.001
zm0 <- geomH(z, 0) * 0.001
zm80 <- geomH(z, 80) * 0.001

zGP <- z * 0.001
T <- 288.15 - 6.5 * zGP
T[zGP > 11] <- 216.65
T[zGP > 20] <- 216.65 + 1.0 * (zGP[zGP > 20] - 20)
T[zGP > 32] <- 228.65 + 2.8*(zGP[zGP > 32] - 32)
T[zGP > 47] <- 273.15 - 2.5

gamma <- -0.0065
Tzero <- 288.15
pzero=1013.25
gzero <- 9.80665
Rd <- 8314.32 / 28.9644
alpha <- -Rd*gamma/gzero
K <- 1.313e-5
Cmb2inHg <- 25.4 * 1.33322387415
PA <- function (zs,ps) {pzero*(-gamma*zs/Tzero+(ps/pzero)^alpha)^(1/alpha)}
PA2 <- function (zs,ps) {(ps^alpha+K*zs/0.3048)^(1/alpha)}  ## ps in inHg; also, result
PA3 <- function (zs,ps) {ps*(1-(pzero^alpha*gamma/Tzero)*(zs/ps^alpha))^(1/alpha)}

stns <- c(72469,72520,91285,72393,72202,72426,72764,72786,72365,70273)
names(stns) <- c('Denver', 'Pittsburg', 'Hilo', 'Vandenberg', 'Miami', 'Wilmington IL', 'Bismarck', 'Spokane',
                 'Albuquerque', 'Anchorage')

## This function fetches a sounding from the U Wyoming data base:
FetchSounding <- function (year, month, day, ztime, station) {
    Sdata <- sprintf ('soundingData/UWsounding%d%d%02d%02d%02d.Rdata', year, month, day, ztime, station)
    # print (Sdata)
    if (file.exists (Sdata)) {
      load(Sdata)
      if (grepl ('Invalid', Z[1]) || grepl('Can\'t', Z[2])) {
        # print ('invalid request')
        return (NA)
      }
    } else {
      URLtoFetch <- paste("http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST",
        sprintf('&YEAR=%d&MONTH=%d&FROM=%02d%02d&TO=%02d%02d&STNM=%d',
          year, month, day, ztime, day, ztime, station), sep='')
      # print (sprintf ('fetching sounding from %s', URLtoFetch))
      con <- curl::curl (URLtoFetch)
      Z <- readLines(con)
      close(con)
      # print (sprintf ('Z[1]=%s', Z[1]))
      if (grepl ('Invalid', Z[1]) || grepl('Can\'t', Z[2])) {
        # print ('invalid request')
        return (NA)
      } else {
        save (Z, file=Sdata)
      }
    }
  ## get the station latitude
  isl <- which(grepl('Station latitude', Z))
  if (length(isl) == 1) {
    stationLatitude <- as.numeric (gsub('Station latitude:', '', Z[isl]))
    assign ('stationLatitude', stationLatitude, pos='.GlobalEnv')
  }
  i <- 1
  while (!grepl ('------------------------', Z[i])) {i <- i+1}
  i <- i + 1
  if (grepl ('PRES', Z[i])) {
    i <- i + 2
    s <- rep(' ', 2)
    while (length(s) < 5) {
      i <- i + 1
      Z[i] <- gsub (' $', '', gsub ('^ ', '', gsub('\\s +', ' ', Z[i])))
      s <- strsplit (Z[i], ' ')[[1]]
    } 
    i1 <- i  ## starting value
    while (!grepl ('[a-zA-Z]', Z[i])) {
      i <- i+1
      Z[i] <- gsub (' $', '', gsub ('^ ', '', gsub('\\s +', ' ', Z[i])))
    }
    i2 <- i - 1  ## final value
  } else {
    # print ('error 1 encountered')
  }
  ## make data.frame
  SND <- data.frame()
  Pressure <- vector()
  Height <- vector()
  Temperature <- vector()
  DewPoint <- vector()
  spsv <- 0
  for (i in i1:i2) {
    s <- strsplit (Z[i], ' ')[[1]]
    if (s[1] == spsv) {next}  ## skip duplicates
    spsv <- s[1]
    Pressure <- c(Pressure, as.numeric(s[1]))
    Height <- c(Height, as.numeric(s[2]))
    Temperature <- c(Temperature, as.numeric(s[3]))
    DewPoint <- c(DewPoint, as.numeric(s[4]))
  }
  SND <- data.frame(Pressure, Temperature, DewPoint, Height)
  SND$EWX <- MurphyKoop (SND$DewPoint, SND$Pressure)
  SND$MR <- MixingRatio (SND$EWX / SND$Pressure)
  SND$TVIR <- VirtualTemperature (SND$Temperature, SND$MR)+273.15
  SND$PALT <- PressureAltitude (SND$Pressure)
  return (SND)
}

ui <- fluidPage(
   
   # Application title
   titlePanel("Geopotential Height and the Standard Atmosphere"),
     tabsetPanel (id='whichTab', type='pills',
     tabPanel ('Objective',
       includeHTML('HTML/GeopotentialHeightA.html')
       ),
     tabPanel ('ISA Calculator',
       sidebarLayout(
         sidebarPanel(
           numericInput('pressure', 'enter pressure [hPa]', min=1, max=1100, value=500),
           textOutput('palt'),
           numericInput('zalt', 'enter pressure altitude [m]', min=-100, max=50000, value=0),
           textOutput('prssr'),
           includeHTML('HTML/ISAcalculator.html')
         ),
       mainPanel (
         imageOutput('ISAplot')
       )
       )
       ),
     tabPanel ('Height Converter',
   sidebarLayout(
      sidebarPanel(
        fluidRow(
          column (6, numericInput("latitude", "latitude [degrees]:",
                     min = 0, max = 90, value = 45, step=1)),
          column (6, numericInput('height', 'height [m]', value=15000))),
        fluidRow (
          column (6, h5('geoid height: conventionally, 0. See the reference below.')),
          column (6, numericInput('geoid', 'geoid height [m]', value=0))
        ),
        radioButtons('which', 'conversion direction', 
          choices=c('geometric to geopotential Z', 'geopotential to geometric z')),
        textOutput('result'),
        includeHTML('HTML/GeopotentialZcalculator.html')
      ),
      
      mainPanel(
         plotlyOutput("cPlot"),
         includeHTML('HTML/FollowUp.html')
      )
   )),
       tabPanel ('Explore Soundings',
         sidebarLayout(
           sidebarPanel(
             fluidRow (
               column (4, selectInput ('sndYear', 'year', choices=c(2017, 2016), selected=2017)),
               column (4, selectInput ('sndMonth', 'month', choices=c(1:12), selected=10)),
               column (4, numericInput ('sndDay', 'day', min=1, max=31, value=23))),
             fluidRow (
               column (6, selectInput ('sndTime', 'Z time', choices=c(0,12), selected=12)),
               column (6, selectInput ('sndStation', 'station', choices=stns))),
             includeHTML('HTML/SoundingExplorer.html')),
           mainPanel(
             plotOutput("ePlot"),
             plotOutput ('difPlot')
           )
         )),
       tabPanel ('The Altimeter Setting',
         sidebarLayout(
           sidebarPanel(
         includeHTML('HTML/Altimeter.html')),
           mainPanel(
             fluidRow (
               column (3, numericInput ('ps', 'station pressure [hPa]', value=885)),
               column (3, numericInput ('zs', 'station elevation [m]', value=1350)),
               column (3, checkboxInput ('psadj', 'subtract 0.3 hPa?', value=TRUE))),
             fluidRow(
               column (5, sliderInput ('alt', 'altimeter setting [inHg]', 
                                       min=25, max=35, value=30.71, step=0.01)),
               column (2, textOutput ('altS'))),
             plotOutput ('altPlot', width=600, height=450)
           ))
         # sidebarLayout(
         #   sidebarPanel(
         #     numericInput("latitude",
         #       "latitude [degrees]:",
         #       min = 0,
         #       max = 90,
         #       value = 45,
         #       step=5),
         #     numericInput('height', 'height [m]', value=15000),
         #     radioButtons('which', 'conversion direction', choices=c('to geopotential Z', 'to geometric z')),
         #     textOutput('result'),
         #     includeHTML('HTML/GeopotentialZcalculator.html')
         #   ),
         #   
         #   mainPanel(
         #     plotlyOutput("cPlot")
         #   )
         # )
       #)
  #)
     )
       )
)

server <- function(input, output, session) {
  
  getSounding <- reactive ({
    input$sndStation
    # print ('entered getSounding')
    SND <<- FetchSounding (as.integer(input$sndYear), as.integer(input$sndMonth), as.integer(input$sndDay),
                   as.integer(input$sndTime), as.integer(input$sndStation))
    # print (str(SND))
    if (!is.data.frame(SND)) {
      showModal(modalDialog(
        'invalid request; loading default case',
        title = "Error Fetching Sounding",
        easyClose = TRUE
      ))
      SND <<- FetchSounding (2017, 10, 23, 12, 72469)
      updateSelectInput(session, 'sndYear', selected=2017)
      updateSelectInput(session, 'sndMonth', selected=10)
      updateNumericInput(session, 'sndDay', value=23)
      updateSelectInput(session, 'sndTime', selected=12)
      updateSelectInput(session, 'sndStation', selected=72469)
    }
    pmin <- ceiling(min(SND$Pressure, na.rm=TRUE))
    pmax <- floor(max(SND$Pressure, na.rm=TRUE))
    p_interp <- pmin:(pmax-1) + 0.5
    t_interp <- vector('numeric', pmax-pmin)
    tv_interp <- vector ('numeric', pmax-pmin)
    z_interp <- vector ('numeric', length(p_interp))
    for (i in pmin:(pmax-1)) {
      tv_interp[i-pmin+1] <- LagrangeInterpolate (i+0.5, 3, 
        data.frame (SND$Pressure, SND$TVIR))
      t_interp[i-pmin+1] <- LagrangeInterpolate (i+0.5, 3, 
        data.frame (SND$Pressure, SND$Temperature))
      z_interp[i-pmin+1] <- LagrangeInterpolate (i+0.5, 3,
        data.frame (SND$Pressure, SND$Height))
    }
    t_interp <- t_interp + 273.15
    ## find height increments in each pressure interval:
    dz <- 287.05 * tv_interp / gzero * c(diff (log (p_interp)), 0)
    dz2 <- 287.05 * t_interp / gzero * c(diff (log (p_interp)), 0)
    # print (sprintf ('using %f for station latitude', stationLatitude))
    dz3 <- 287.05 * tv_interp / Gravity (stationLatitude, z_interp) * c(diff (log (p_interp)), 0)
    p_interp <- rev (p_interp)
    t_interp <- rev(t_interp)
    tv_interp <- rev(tv_interp)
    dz <- rev (dz)
    dz2 <- rev (dz2)
    dz3 <- rev (dz3)
    H <<- cumsum (dz) + SND$Height[1]
    H2 <<- cumsum (dz2) + SND$Height[1]
    H3 <<- cumsum (dz3) + SND$Height[1]
    isa_temp <- 288.15 - 0.0065 * H2
    isa_temp[H2 > 11000] <- 216.65
    isa_temp[H2 > 20000] <- 216.65 + 0.001 * (H2[H2 > 20000] - 20000)
    p_interp <<- p_interp
    isa_temp <<- isa_temp
    t_interp <<- t_interp
    tv_interp <<- tv_interp
    H4 <<- PressureAltitude (p_interp)
    return(SND)
  })
  
  output$altPlot <- renderPlot ({
   
    ## construct an axis representing the standard atmosphere
    zp <- seq(0,2,0.5)
    xp <- rep(0,length(zp))
    pp <- pzero * (1-gamma*zp*1000/Tzero)^(1/alpha)
    plot(xp,zp, axes=FALSE, xlab=NA, ylab=NA, col=NA, xlim=c(0,1))
    xo <- 0.2
    axis(side=2, pos=xo)
    p <- c(1000, 950, 900, 850, 800)
    z <- PressureAltitude(p)*0.001
    for (i in 1:length(z)) {
      lines(c(xo,xo+0.05), c(z[i],z[i]))
      text(xo+0.05,z[i],labels=sprintf('%.0f',p[i]), adj=0.0)
    }
    lines(c(xo,xo+0.05), c(0,0))
    text(xo+0.05, 0, labels='1013.25', pos=4)
    zs <- input$zs / 1000
    ps <- input$ps
    points(xo-0.01,zs,pch=20,col='black')
    text(xo-0.02,zs,"zs", pos=2, col='black')
    zpp <- PressureAltitude(ps)*0.001
    points(xo+0.01, zpp, pch=20, col='blue')
    text(xo+0.02, zpp, 'ps', pos=4, col='blue')
    ## now construct another axis with adjustment
    if (input$psadj) {
      zpp <- PressureAltitude (ps-0.3) * 0.001
    }
    xo <- xo + 0.5
    axis(side=2, pos=xo)
    baseP <- input$alt * Cmb2inHg
    ## find the corresponding height
    zbase <- PressureAltitude(baseP)
    # dz <- zs+Tzero/gamma*(1-(ps/pzero)^alpha)*0.001
    dz <- -zbase*0.001
    for (i in 1:length(z)) {
      lines(c(xo,xo+0.05), c(z[i]+dz,z[i]+dz))
      text(xo+0.05,z[i]+dz,labels=sprintf('%.0f',p[i]), adj=0.0)
    }
    points(xo-0.01,zs,pch=20,col='black')
    # lines(c(0,0.6), c(zs,zs), col='black', lty=3)
    text(xo-0.02,zs,"zs", pos=2, col='black')
    points(xo+0.01, zpp+dz, pch=20, col='blue')
    text(xo+0.02, zpp+dz, 'ps', pos=4, col='blue')
    pa <- PA(zs*1000, ps)
    lines(c(xo,xo+0.05), c(0,0), col='blue', lwd=2)
    text(xo+0.05, 0, labels=sprintf('%.1f', baseP), pos=4, col='blue')
    lines(c(xo,xo+0.05), c(dz,dz))
    text(xo+0.05, dz, labels='1013.25', pos=4)
    text(0.0,1,'pressure altitude [km]', srt=90, cex=2)
    text(xo+.25,1.1,'pressure [hPa]', srt=270, cex=2)
    title(sprintf('Altimeter setting: %.2f', input$alt))
  }) 
  
  output$result <- renderText ({
    zin <- as.numeric(input$height)
    if (grepl ('to geopotential', input$which)) {
      zout <- GeoPotHeight (input$latitude, zin, as.numeric(input$geoid))
      sprintf ('%.2f geometric m = %.2f geopotential m', zin, zout)
    } else {
      zout <- geomH (zin, input$latitude, input$geoid)
      sprintf ('%.2f geopotential m = %.2f geometric m', zin, zout)
    }
  })
  
  output$altS <- renderText ({
    zs <- as.numeric (input$zs)
    ps <- as.numeric (input$ps)
    if (input$psadj) {ps <- ps - 0.3}
    alpha <- StandardConstant ('Rd') * 0.0065 / StandardConstant ('g_standard')
    as <- (0.0065 * zs / 288.15 + (ps/1013.25)^alpha)^(1/alpha) * 29.92126
    sprintf ('alt setting %.3f', as)
  })
   
  output$palt <- renderText({
    p <- as.numeric(input$pressure)
    zout <- PressureAltitude (p)
    sprintf ('%.2f m', zout)
  })
  
  output$prssr <- renderText({
    zin <- as.numeric(input$zalt)
    minf <- function (p) {
      return (PressureAltitude(p)-zin)
    }
    ps <- 500.
    sprintf ('%.2f hPa', nleqslv(ps, minf)$x)
  })
  
  output$ISAplot <- renderImage({
    list(src = 'ISAplot.png',
      contentType = 'image/png',
      width = 800,
      height = 600,
      alt = "ISA image")
  }, delete=FALSE)
  
   output$cPlot <- renderPlotly({
     zm <- geomH(z, input$latitude, input$geoid) * 0.001
     DF <- data.frame(zGP=zGP, zGM=zm, difference=1000*(zm-zGP))
     g <- ggplot(data=DF) + geom_path (aes(x=zGP, y=difference, label=zGM))
     g <- g + xlab('geopotential height [km]') + 
              ylab('geometric - geopotential height [m]') +
              theme_WAC(1)
     ggplotly(g)
   })
   
   output$ePlot <- renderPlot({
     input$sndYear; input$sndMonth; input$sndDay; input$sndTime; input$sndStation
     SND <- getSounding()
     # SND <<- FetchSounding (as.integer(input$sndYear), as.integer(input$sndMonth), as.integer(input$sndDay), 
     #                as.integer(input$sndTime), as.integer(input$sndStation))
     
     DF <- data.frame(Pressure=p_interp, GeopotentialHeight=H/1000, DryGeopHeight=H2/1000, 
                      GeometricHeight=H3/1000, ISA=H4/1000)
     cLines <- c('blue', 'forestgreen', 'black', 'cyan')
     names(cLines) <- c('GeometricHeight', 'DryGeopHeight', 'GeopotentialHeight', 'ISA')
     grid.newpage()
     vp1 <- viewport(width=0.52, height=1, x=0.26, y=0.5)
     vp2 <- viewport(width=0.48, height=0.995, x=0.76, y=0.503)
     xmax <- max(p_interp, na.rm=TRUE)
     if (xmax < 1000) {xmax <- 1000}
     xmin <- min (p_interp, na.rm=TRUE)
     if (xmin > 10) {xmin <- 10}
     g1 <- ggplot (data=DF) + geom_path (aes(x=Pressure, y=GeometricHeight, colour='GeometricHeight')) +
       ylab('height [km]') + xlab('pressure [hPa]') +
       geom_path (aes(x=Pressure, y=DryGeopHeight, colour='DryGeopHeight')) +
       geom_path (aes(x=Pressure, y=GeopotentialHeight, colour='GeopotentialHeight')) +
       geom_path (aes(x=Pressure, y=ISA, colour='ISA')) +
       scale_colour_manual (name='', values=cLines) +
       scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x, n=3), limits = c(xmin, xmax),
         labels = trans_format("log10", math_format(10^.x))) + 
       annotation_logticks(sides='b') + 
       theme_WAC()
     # ggplotly(g1)
     print (g1, vp=vp1)
     c2Lines <- c('blue', 'forestgreen', 'cyan')
     names(c2Lines) <- c('Temperature', 'Virtual Temperature', 'ISA Temperature')
     g2 <- ggplot(data=data.frame(Pressure=p_interp, Temperature=t_interp, TVIR=tv_interp, ISATemperature=isa_temp, 
                                  GeometricHeight=H3/1000)) +
           geom_path (aes(x=Temperature, y=GeometricHeight, colour='Temperature')) +
           geom_path (aes(x=TVIR, y=GeometricHeight, colour='Virtual Temperature')) +
           geom_path (aes(x=ISATemperature, y=GeometricHeight, colour='ISA Temperature')) +
           scale_colour_manual (name='', values=c2Lines) + xlab ('temperature [C]') + 
           ylab ('geometric height [km]') +
           theme_WAC()
     print (g2, vp=vp2)
  })
     
  output$difPlot <- renderPlot ({
    input$sndYear; input$sndMonth; input$sndDay; input$sndTime; input$sndStation
    SND <- getSounding ()  ## need call even though SND not used, because this updates H,H2,etc
    # SND <<- FetchSounding (as.integer(input$sndYear), as.integer(input$sndMonth), as.integer(input$sndDay), 
    #   as.integer(input$sndTime), as.integer(input$sndStation))
    STATION <- input$sndStation
    S <- names(stns[which(stns == STATION)])
    YR <- as.integer(input$sndYear)
    MONTH <- as.integer(input$sndMonth)
    DAY <- as.integer(input$sndDay)
    ZTIME <- as.integer(input$sndTime)
     H4 <- PressureAltitude(p_interp)
     npl <- ifelse (min(p_interp, na.rm=TRUE) > 80, 2, 3)
     xmax <- max(p_interp, na.rm=TRUE)
     if (xmax < 1000) {xmax <- 1000}
     xmin <- min (p_interp, na.rm=TRUE)
     if (xmin > 10) {xmin <- 10}
     plotTitle <- sprintf ('%s %d/%02d/%02d %02dZ', S, YR, MONTH, DAY, ZTIME)
     g3 <- ggplotWAC(data.frame(Pressure=p_interp, Geopotential=H-H3, DryGeop=H2-H3, Dvalue=H-H4, ISA=H4-H3),
       col=c('blue', 'forestgreen', 'black', 'cyan'),
       ylab='difference vs. geometric [m]') +
       xlab('pressure [hPa]') +
       scale_x_log10(breaks = trans_breaks("log10", (function(x) 10^x), n=3), limits = c(xmin, xmax),
         labels = trans_format("log10", math_format(10^.x))) +
       annotation_logticks(sides='b') + 
       ggtitle(plotTitle) +
       theme(legend.position=c(0.74,-0.05)) 
     print (g3)
   })
   
}

# Run the application 
shinyApp(ui = ui, server = server)

