library(shiny)
library(plotly)
##-----------------------------------------------------------------------------
ui <- pageWithSidebar(headerPanel('PDP Visualizer'),
                      sidebarPanel(
                        fileInput('data', 'Select a CSV'),
                        selectInput('xval', 'X variable', choices = c('1','2','3')),
                        selectInput('yval', 'Y variable', choices = c('1','2','3'), selected = 'Temperature'),
                        sliderInput('smooth', 'Degree of Smoothing', min = 0, max = 1, value = 0.5, step = 0.01)),
                      mainPanel(
                        plotlyOutput('plot')
                      )
)






library(shiny)

server <- shinyServer(function(input, output,session) {
  ##---------------------------------------------------------------------------
  ## load the data
  filedata <- reactive({
    infile <- input$data
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    temp <- read.csv(infile$datapath)
  })
  ##---------------------------------------------------------------------------
  spline <- reactive({
    infile <- input$data
    if (is.null(infile))
      # User has not uploaded a file yet. Use NULL to prevent observeEvent from triggering
      return(NULL)
    if(!is.null(infile)){
      smooth.spline(filedata()[, input$xval], filedata()[, input$yval], spar = input$smooth)}
  })
  ##---------------------------------------------------------------------------
  ## update x value menu
  observeEvent(filedata(), {
    updateSelectInput(session, inputId =  "xval", choices = colnames(filedata()))
  })
  
  ##---------------------------------------------------------------------------
  ## update y value menu
  observeEvent(filedata(), {
    updateSelectInput(session, inputId =  "yval", choices = colnames(filedata()))
  })
  
  ##---------------------------------------------------------------------------
  
  
  ##---------------------------------------------------------------------------
  ## plot the selected variables
  output$plot <- renderPlotly({
    plot_ly(x = filedata()[, input$xval], 
            y = filedata()[, input$yval], 
            mode = 'lines', 
            type = 'scatter',
            name = input$yval) %>%
      add_lines( x = filedata()[, input$xval],
                 y = fitted(spline()),
                 name = 'spline fit')
    # add_lines(x = filedata()[, input$xcol],
    #                     y = as.numeric(fitted(spline())))
  })
})


shinyApp(ui = ui, server = server)
