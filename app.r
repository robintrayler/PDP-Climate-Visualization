library(shiny)
library(plotly)
library(shinythemes)
## Set up UI-------------------------------------------------------------------
ui <- fluidPage(
  ## Set theme-----------------------------------------------------------------
  theme = shinythemes::shinytheme("journal"), # select a theme
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
    sliderInput(inputId = 'smooth', # slider for degree of smooting
                label = 'Degree of Smoothing', 
                min = 0, 
                max = 1, 
                value = 0.5, 
                step = 0.01)),
  ## Set main panel------------------------------------------------------------
  mainPanel( # set up a tabbed panel 
    tabsetPanel(
      tabPanel('Plot', plotlyOutput('plot')), # tab for plot
      tabPanel('Data', tableOutput('table'))) # tabl for data
  )
)

## Set up Server---------------------------------------------------------------
server <- shinyServer(function(input, output,session) {
  
  ## Load Data-----------------------------------------------------------------
  ## load the data
  filedata <- reactive({
    infile <- input$data
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    tbl <- read.csv(infile$datapath)
    return(tbl)
  })
  
  ## Spline Fit----------------------------------------------------------------
  spline <- reactive({ # spine fit uses slider input to control smooting
    infile <- input$data
    if(!is.null(infile)){
      smooth.spline(filedata()[, input$xval], filedata()[, input$yval], spar = input$smooth)}
  })
  
  ## Update Menu Options-------------------------------------------------------
  ## update menu options once data is loaded
  ## update x value menu
  observeEvent(filedata(), {
    updateSelectInput(session, inputId =  "xval", choices = colnames(filedata()))
  })
  ## update y value menu
  observeEvent(filedata(), {
    updateSelectInput(session, inputId =  "yval", choices = colnames(filedata()))
  })
  
  ## Plot selected variables---------------------------------------------------
  output$plot <- renderPlotly({
    plot_ly(x = filedata()[, input$xval], 
            y = filedata()[, input$yval], 
            mode = 'lines', 
            type = 'scatter',
            name = input$yval) %>%
      add_lines( x = filedata()[, input$xval],
                 y = fitted(spline()),
                 name = 'spline fit')
  })
  ## Add table to second tab---------------------------------------------------
  output$table <- renderTable(filedata())
  
})

shinyApp(ui = ui, server = server)
