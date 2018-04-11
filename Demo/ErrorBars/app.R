#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(Ranadu)
Project <- 'CSET'
Flight <- 1
Data <- getNetCDF(sprintf('%s%s/%srf%02d.nc', DataDirectory(), Project, Project, Flight),
              standardVariables(c('ATH1', 'ATH2', 'ATF1', 'AT_A')))

# Define UI 
ui <- fluidPage(
   
   # Application title
   titlePanel("ErrorBarPlot"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        numericInput('fno', label='Flight', min=1, max=10, value=1),
         sliderInput("bins",
                     "Number of bins:",
                     min = 10,
                     max = 50,
                     value = 20),
         radioButtons('sig', label='type error bar', choices=c('std dev', '2 std dev', 'sd of mean'))
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("errorbarPlot",height=600)
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$errorbarPlot <- renderPlot({
     Data <- getNetCDF(sprintf('%s%s/%srf%02d.nc', DataDirectory(), Project, Project, input$fno),
                       standardVariables(c('ATH1', 'ATH2', 'ATF1', 'AT_A')))
     
           # generate bins based on input$bins from ui.R
      B <- binStats(data.frame(Data$ATH1-Data$ATH2, Data$ATX), bins=input$bins)
      if (input$sig == '2 std dev') {
        B$sigma <- B$sigma * 2
      } else if (input$sig == 'sd of mean') {
        B$sigma[B$nb < 3] <- NA
        B$sigma <- B$sigma / sqrt(B$nb)
      }
      g <- ggplot(data=B)+geom_errorbar(aes(x=xc, ymin=ybar-sigma, ymax=ybar+sigma), color='blue')
      g <- g + geom_point(aes(x=xc, y=ybar), color='blue', size=4)
      g <- g + xlab('ATX') + ylab('ATH1-ATH2')
      g + theme_WAC()
      
      # draw the histogram with the specified number of bins
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

