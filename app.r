## Visualization tool for PDP climate exercise
## Robin B. Trayler May 6, 2019
## Load required libraries-----------------------------------------------------
library(shiny)
library(plotly)
library(shinythemes)
## Set up UI-------------------------------------------------------------------
ui <- fluidPage(
  ## Set theme-----------------------------------------------------------------
  theme = shinythemes::shinytheme("flatly"), # select a theme
  # shinythemes::themeSelector(), # uncomment to see different themes
  ## Set up header-------------------------------------------------------------
  headerPanel(title = 'CLIMATE WEST!'), # set page title
  ## Set up sidebar------------------------------------------------------------
  sidebarPanel(width = 3,
               selectInput(inputId = 'data',
                           label = 'Select a dataset',
                           choices = tools::file_path_sans_ext(basename(list.files('./data/')))),
               selectInput(inputId = 'data2',
                           label = 'Select a second dataset',
                           choices = tools::file_path_sans_ext(basename(list.files('./data/')))),
               selectInput(inputId = 'xval', # drop down for plotting
                           label = 'X variable', 
                           choices = c('1','2','3')),
               selectInput(inputId = 'yval', # drop down for plotting
                           label = 'Y variable', 
                           choices = c('1','2','3')),
               selectInput(inputId = 'pt.type', # drop down for plotting
                           label = 'Plot Type', 
                           choices = c('markers','lines'))),
  
  ## Set main panel------------------------------------------------------------
  mainPanel( # set up a tabbed panel 
    tabsetPanel(
      tabPanel('Plot', 
               plotlyOutput('plot',
                            height = '650px', 
                            width = '115%')), # tab for plot
      # tabPanel('Location', imageOutput('image')), 
      tabPanel('Dataset 1', tableOutput('table')),
      tabPanel('Dataset 2', tableOutput('table2'))#,
      # tabPanel('error',verbatimTextOutput('error'))
    )
  )
)


## Set up Server---------------------------------------------------------------
server <- shinyServer(function(input, output, session) {
  
  ## list data ----------------------------------------------------------------
  
  ## Load Data-----------------------------------------------------------------
  filedata <- reactive({
    tbl <- read.csv(paste0('./data/',input$data,'.csv'))
    return(tbl)
  })
  
  filedata2 <- reactive({
    tbl <- read.csv(paste0('./data/',input$data2,'.csv'))
    return(tbl)
  })
  
  # filedata2 <- reactive({
  #   infile <- input$data2
  #   if (is.null(infile))
  #     # if nothing uploaded use NULL to prevent observeEvent from triggering
  #     return(NULL)
  #   tbl <- read.csv(infile$datapath)
  #   return(tbl)
  # })
  
  ## Spline Fit----------------------------------------------------------------
  spline <- reactive({ # spine fit uses slider input to control smooting
    # infile <- input$data
    # if(!is.null(infile)){
    smooth.spline(filedata()[, input$xval], 
                  filedata()[, input$yval], 
                  spar = 0.5)
    # }
  })
  # )
  linear_regression <- reactive({# spine fit uses slider input to control smooting
    # infile <- input$data
    # if(!is.null(infile)){
    coeffs <- lm(filedata()[, input$yval] ~ filedata()[, input$xval]) %>% coefficients()
    regression <- coeffs[2] * filedata()[, input$xval] + coeffs[1]
    # }
  })
  
  ## Spline Fit2----------------------------------------------------------------
  spline2 <- reactive({ # spine fit uses slider input to control smooting
    infile <- input$data2
    if(!is.null(infile)){
      smooth.spline(filedata2()[, input$xval], 
                    filedata2()[, input$yval], 
                    spar = 0.5)}
  })
  
  linear_regression2 <- reactive({# spine fit uses slider input to control smooting
    infile <- input$data2
    if(!is.null(infile)){
      coeffs <- lm(filedata2()[, input$yval] ~ filedata2()[, input$xval]) %>% coefficients()
      regression <- coeffs[2] * filedata2()[, input$xval] + coeffs[1]
    }
  })
  
  # linear_regression2_coeff <- reactive({# spine fit uses slider input to control smooting
  #   infile <- input$data2
  #   if(!is.null(infile)){
  #     coeffs <- lm(filedata2()[, input$yval] ~ filedata2()[, input$xval]) %>% coefficients()
  #     text = as.character(paste('Y = ', coeffs[2],'* X +',coeffs[1]))
  #   }
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
    plt <- plot_ly(x = filedata()[, input$xval], 
                   y = filedata()[, input$yval], 
                   mode = input$pt.type, 
                   type = 'scatter',
                   name = paste(input$data)) %>%
      add_lines(x = filedata()[, input$xval],
                y = fitted(spline()),
                name = 'smooth trend',
                line = list(width = 4),
                visible = "legendonly") %>% 
      add_lines(x = filedata()[, input$xval], 
                y = linear_regression(),
                name = 'linear trend',
                line = list(width = 4),
                visible = "legendonly")
    
    if (!is.null(filedata2())) {
      plt <- add_trace(plt, x = filedata2()[, input$xval],
                       y = filedata2()[, input$yval],
                       mode = input$pt.type,
                       type = 'scatter',
                       name = paste(input$data2),
                       visible = "legendonly") %>%
        add_lines(x = filedata2()[, input$xval],
                  y = fitted(spline2()),
                  name = 'smooth trend',
                  line = list(width = 4),
                  visible = "legendonly") %>% 
        add_lines(x = filedata2()[, input$xval], 
                  y = linear_regression2(),
                  name = 'linear trend',
                  line = list(width = 4),
                  visible = "legendonly")
    }
    plt <- layout(plt, 
                  xaxis = list(title = input$xval),
                  yaxis = list(title = input$yval))
    plt
    
  })
  ## Add table to second tab---------------------------------------------------
  output$table <- renderTable(filedata())
  output$table2 <- renderTable(filedata2())
  # output$sidebarText <- renderText('hello')
  # output$error <- renderText(paste('./',input$dataset))
  ##---------------------------------------------------------------------------
})

shinyApp(ui = ui, server = server)
rsconnect::deployApp('~/Dropbox/PDP-Climate-Visualization/')


