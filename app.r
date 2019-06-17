## Visualization tool for PDP climate exercise
## Robin B. Trayler May 6, 2019
# Nonparametric Smoothing Function --------------------------------------------
nonparametric_smooth <- function(x, y, xmod = x, winsize){
  ## INPUTS
  ## x = x values
  ## y = yvalues
  ## xmod = positions to calculate statistics at. default is data positions
  ## winsize = standard deviation for gaussian kernel
  ## OUTPUTS
  ## mean = ymod = the weighted moving mean
  ymod <- vector(length = length(xmod)) # preallocate
  for (i in 1:length(xmod)) { # for each value in xmod
    w <- dnorm(x, xmod[i], winsize/2) # weights 
    ymod[i] <- sum(w * y) / sum(w) # calculate the moving weighted mean
  }
  
  return (list(mean = ymod)) # return the results
}
## Load required libraries-----------------------------------------------------
library(shiny)
library(plotly)
library(shinythemes)
## Set up UI-------------------------------------------------------------------
ui <- fluidPage(
  ## Set theme-----------------------------------------------------------------
  theme = shinythemes::shinytheme("paper"), # select a theme
  # shinythemes::themeSelector(), # uncomment to see different themes
  ## Set up header-------------------------------------------------------------
  headerPanel(title = 'PDP Visualization'), # set page title
  
  ## Set up sidebar------------------------------------------------------------
  sidebarPanel(
    fileInput(inputId = 'data', # allow a csv to be loaded
              label = 'Select a CSV'),
    selectInput(inputId = 'xval', # drop down for plotting
                label = 'X variable', 
                choices = c('1','2','3')),
    selectInput(inputId = 'yval', # drop down for plotting
                label = 'Y variable', 
                choices = c('1','2','3')),
    selectInput(inputId = 'pt.type', # drop down for plotting
                label = 'Plot Type', 
                choices = c('markers','lines')),
    sliderInput(inputId = 'smooth', # slider for degree of smooting
                label = 'Spline Degree of Smoothing', 
                min = 0, 
                max = 1, 
                value = 0.5, 
                step = 0.01),
    sliderInput(inputId = 'smooth2', # slider for degree of smooting
                label = 'Moving Average Window Size (x axis units)', 
                min = 0, 
                max = 50, 
                value = 5, 
                step = 1)),
  
  
  ## Set main panel------------------------------------------------------------
  mainPanel( # set up a tabbed panel 
    tabsetPanel(
      tabPanel('Plot', plotlyOutput('plot')), # tab for plot
      tabPanel('Data', tableOutput('table'))#,# tab for data
      # tabPanel('Error', textOutput('err'))
      # tabPanel('Predictions', textOutput('text'), verbatimTextOutput("value")) 
    )
  )
)

## Set up Server---------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  
  ## Load Data-----------------------------------------------------------------
  ## load the data
  filedata <- reactive({
    infile <- input$data
    if (is.null(infile))
      # if nothing uploaded use NULL to prevent observeEvent from triggering
      return(NULL)
    tbl <- read.csv(infile$datapath)
    return(tbl)
  })
  
  ## Spline Fit----------------------------------------------------------------
  spline <- reactive({ # spine fit uses slider input to control smooting
    infile <- input$data
    if(!is.null(infile)){
      smooth.spline(filedata()[, input$xval], 
                    filedata()[, input$yval], 
                    spar = input$smooth)}
  })
  
  average <- reactive({ # spine fit uses slider input to control smooting
    infile <- input$data
    if(!is.null(infile)){
      nonparametric_smooth(x = filedata()[, input$xval], 
                           y = filedata()[, input$yval], 
                           winsize = input$smooth2)}
  })
  ## calculate error-----------------------------------------------------------
  # error <- reactive({
  #   infile <- input$data
  #   if(!is.null(infile)) {
  #     I <- length(filedata()[, input$xval])
  #     er <- vector(length = I)
  #     for(i in 1:I){
  #       fit <- nonparametric_smooth(filedata()[-i,input&xval], filedata()[-i,input&yval], winsize = input$smooth2)
  #       f <- approxfun(x = filedata()[-i,input&xval], y = fit$mean)
  #       err[i] <- (f(filedata()[i,input&xval]) - filedata()[i,input&yval])
  #     }
  #     sqrt((mean(err[!is.na(err)]^2)))
  #   }
  #   verbatimTextOutput(sqrt((mean(err[!is.na(err)]^2))))
  # })
  
  ## Update Menu Options-------------------------------------------------------
  ## update menu options once data is loaded
  ## update x value menu
  observeEvent(filedata(), {
    updateSelectInput(session, 
                      inputId =  "xval", 
                      choices = colnames(filedata()))
  })
  ## update y value menu
  observeEvent(filedata(), {
    updateSelectInput(session, 
                      inputId =  "yval", 
                      choices = colnames(filedata()))
  })
  
  ## Plot selected variables---------------------------------------------------
  output$plot <- renderPlotly({
    plot_ly(x = filedata()[, input$xval], 
            y = filedata()[, input$yval], 
            mode = input$pt.type, 
            type = 'scatter',
            name = input$yval) %>%
      add_lines(x = filedata()[, input$xval],
                y = fitted(spline()),
                name = 'spline fit') %>%
      add_lines(x = average()$xmod, 
                y = average()$mean,
                name = paste(input$smooth2, 'year moving average', sep = ' '))
  })
  ## Add table to second tab---------------------------------------------------
  output$table <- renderTable(filedata())
  # output$err <- renderText(error())
  
  ##---------------------------------------------------------------------------
})

shinyApp(ui = ui, server = server)
# rsconnect::deployApp('~/Dropbox/PDP-Climate-Visualization/')