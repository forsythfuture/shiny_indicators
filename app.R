#
# This app is a test app for the updated indicators app
#

library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(kableExtra)

# load all custom functions
source('global.R')

# text to paste on significance testing table name
sig_text <- ' test of significance'

# web address to GitHub folder containg datasets that can be used in shiny
github_datasets <- 'https://github.com/forsythfuture/indicators/tree/master/shiny_datasets'

ui <- dashboardPage(
  
  dashboardHeader(title = 'Indicators'),
  dashboardSidebar(
    
    fileInput("dataset", "Import CSV file:",
              accept = c(
                "text/csv",
                "text/comma-separated-values,text/plain",
                ".csv")
    ),

    tags$p(),
    
    # text box to enter URL to dataset
    textInput('dataset_url', 'Enter URL:'),
    actionButton('submit', 'Submit URL'),
    
    # demographic drop down menu
    # will not display until data file is selected
    selectInput('demographic', 'Demographic:', ""),
    
    tags$p(),
    
    radioButtons("sig_test", "Significance Test:",
                 c("Z-score" = "zscore",
                   "Chi-Square" = "chisquare"),
                 selected = 'zscore'),
    
    # download tableau data buttom
    downloadButton("download_tableau", "Download Tableau")
    
  ), # dashboardSidebar
    
    dashboardBody(
      fluidRow(
        tabsetPanel(
          
          # plots panel
          tabPanel("Plots", 
                   plotlyOutput("plot_line"),
                   plotlyOutput("plot_bar")
          ), # tabPanel for plots
          
          # significance testing panel
          tabPanel('Tests and Estimates',
                   checkboxGroupInput('year_check', 'Years:', choices = "", selected = "", inline = TRUE),
                   checkboxGroupInput('geo_check', 'Geography:', "", selected = 'Forsyth County, NC', inline = TRUE),
                   checkboxGroupInput('demo_check', 'Demographic:', choices = "", inline = TRUE),
                   tags$h4('Significance Tests'),
                   tableOutput('table_sigtest'),
                   tags$p(),
                   tags$h4('Estimates of differences and 95% confidence intervals'),
                   tags$p('Differences are columns minus rows'),
                   tableOutput('table_estimate')
          ), # tabPanel for significance testing
          
          # raw data panel
          tabPanel('Raw Data',
                   dataTableOutput('table_raw_data')
          ), # tabPanel for raw data
          
          # instructions tab panel
          tabPanel('How To Use',
                   tags$h3('How to use the indicators web app'),
                   tags$p('The indicators web app creates plots and conducts significance testing on indicators.  
                          Users only have to upload data in the proper format, and the app will create plots and
                          conduct tests.'),
                   tags$h4('Uploading data'),
                   tags$p('There are two way to upload data:'),
                   tags$ol(
                     tags$li("Upload downloaded file:",
                             tags$p(),
                             tags$p("Go to 'Import CSV file', click on 'Browse...', and then upload a file saved on your local computer.")),
                     tags$li("Import file from a web address:",
                             tags$p(),
                             tags$p("Files can also be imported from web addresses (URLs). More specifically, CSV files can be imported
                                    from GitHub or Google Drive."),
                             tags$ul(
                               tags$li("GitHub: Navigate to the GitHub page for the dataset you want to upload.
                                       Then, click on the 'Raw' button on the top-right side of the header that is 
                                       shown just above the dataset. Finally, copy the URL from the address bar.",
                                       tags$p("A sample of GitHub datasets is available here:"),
                                       tags$a(herf=github_datasets, github_datasets)),
                               tags$p(),
                               tags$li("Google Drive: Right-click on the file and click 'Share'. Then, change the permissions to 
                                       'On - Anyone with the link'.  After changing the file's permissions, copy the sharable link.",
                                       tags$p("Note: Google Drive support is experimental and you may encounter bugs.")))
                               ) # web address list
                   ), # numbered list
                   tags$p(),
                   tags$h4("Formatting Data"),
                   tags$p("For the app to work, the uploaded dataset must be properly formatted. The table below shows an example of the proper formatting"),
                   # table that contains the example format dataset
                   tableOutput('formatting_example'),
                   tags$p("And here is a description of each column:"),
                   tags$ul(
                     tags$li("year:  The year for the given data point in the row."),
                     tags$li("geo_description: The description of the geographic area. For counties, follow the format shown in the table for Forsyth County"),
                     tags$li("type: The type of demograhic group (Race / Ethnicity, Age, Gender, etc.). If the row represents the aggregate for the geographic area
                             then use 'Comparison Community' for type."),
                     tags$li("subtype: The specific demographic group (African American, 18 to 35, Female, etc). Use 'Total' for aggregates of the entire geographic area."),
                     tags$li("estimate: The point estimate of interest (unemployment rate, median wages, crime rate, etc.)."),
                     tags$li("success (optional): Some datasets will take on a binomial form. This takes the form of success / failure or yes/ no.
                             Examples include graduate / not graduate, infant death / non-infant death, or committed a crime / did not commit a crime.
                             These forms of data can be represented in the success and trials columns. Most percentages take this form. Success is the 
                             numerator in the percentile: number of graduates, infant deaths, or crimes."),
                     tags$li('trails (optional): Like success, this column can be used for binomial data. It is the denominator in a percentile and represents 
                             the total number of chances for a success: total students, total births, total population.'),
                     tags$li("se: Standard error of the estimate. Use all zeroes as a placeholder if the data does not contain standard errors .")
                   ) # unordered list
          ) # tab panel for instructions
        ) # tabsetPanel
      ) # fluidRow
    ) # dashboardBody
) # dashboardPage


server <- function(input, output, session) { 
  
  # initialize dataframe that will be created if either
  # a dataset is uploaded or a URL is entered
  df <- reactiveValues(data=NULL)
  
  # initialize object that will create indicator name from file name
  
  # create dataset from file that was uploaded
  observeEvent(input$dataset, {
    
    df <- create_datasets(df, input$dataset$datapath)

  })
  
  # create dataset from URL
  observeEvent(input$submit, {

    df <- create_datasets(df, input$dataset_url)

  })


  # create an updated dataset with the demographic filtered
  df_demo <- eventReactive(input$demographic, {

    # only keep the needed demographic type
    filter(df$data, type == !!input$demographic)

  })
  # 
  # # create demographic drop-down menu
  observe({
    updateSelectInput(session, 'demographic',
                      choices = df$demographics)
  })
  
  # create plots 
  output$plot_line <- renderPlotly({
    if (is.null(df$data)) return()
    plotly_plots(df_demo(), input$demographic, 'line')
  })
  
  output$plot_bar <- renderPlotly({
    if (is.null(df$data)) return()
    plotly_plots(df_demo(), input$demographic, 'bar')
  })
  
  # update significance testing checkboxes based on dataset
  observe({
    if (is.null(df$data)) return()
    updateCheckboxGroupInput(session, 'year_check', choices = unique(df_demo()$year), 
                             selected = max(df$year), inline=TRUE)
    updateCheckboxGroupInput(session, 'geo_check', choices = unique(df_demo()$geo_description), 
                             selected = 'Forsyth County, NC',  inline=TRUE)
    updateCheckboxGroupInput(session, 'demo_check', choices = unique(df_demo()$subtype), 
                             selected = unique(df_demo()$subtype),  inline=TRUE)
  })
  
  # create significance test table
  output$table_sigtest <-  function() {
    if (is.null(df$data)) return()
    df_demo() %>%
      filter(year %in% input$year_check,
             geo_description %in% input$geo_check,
             subtype %in% input$demo_check) %>%
      ff_acs_zscore_kable('estimate', 'se', test = input$sig_test, success = success, trials = trials,
                          var_names = c('year', 'geo_description', 'subtype' ),
                          table_name = if (input$sig_test == 'zscore') paste0('Z', sig_text) else (paste0('Chi-square', sig_text)))

  }
  
  # create estimates and confidence intervals table
  output$table_estimate <-  function() {
    if (is.null(df$data)) return()
    df_demo() %>%
      filter(year %in% input$year_check,
             geo_description %in% input$geo_check,
             subtype %in% input$demo_check) %>%
      estimates_ci_kable('estimate', 'se', test = input$sig_test, success = success, trials = trials,
                         var_names = c('year', 'geo_description', 'subtype' ))
    
  }
  
  # create raw data table
  output$table_raw_data <- DT::renderDataTable({
    if (is.null(df$data)) return()
    ff_data_dt(df_demo(), trials = if (input$sig_test == 'chisquare') TRUE)
  })
  
  # create tableau output
  output$download_tableau <- downloadHandler(
    filename = function() {
      'tableau_indicator.csv'
    },
    content = function(con) {
      write.csv(ff_data_dt(df$data, trials = if (input$sig_test == 'chisquare') TRUE, for_tableau = TRUE),
                con, 
                row.names = FALSE)
    },
    contentType = 'text/csv'
  )
  
  # display dataset that shows the formatting example
  output$formatting_example <- function() {
    
    read_csv('example_format.csv') %>%
      mutate(se = round(se, 2)) %>%
      knitr::kable('html') %>%
      kable_styling('striped', full_width = F, position = 'left')
    
  }
  
}

# Run the application 
shinyApp(ui = ui, server = server)

