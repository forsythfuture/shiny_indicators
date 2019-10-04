plotly_plots <- function(df, input_type, input_geo, plot_type) {
  
  # determine if the dataset is binomial by whether it has a standard error column
  binomial <- if ("se" %in% colnames(df)) FALSE else TRUE
  
  # if there are success and trial columns, create confidence intervals
  if (binomial == TRUE) {
    
    conf_int <- Hmisc::binconf(x = df$success, 
                             n = df$trials,
                             alpha = .05,
                             return.df = T) %>%
                      select(-PointEst)
    
    df <- bind_cols(df, conf_int)
    
  }
  
  # save most recent year as object to be used in plots
  recent_year <- max(df$year)
  
  # create list of margins
  m <- list(
    l = 50,
    r = 50,
    b = 50,
    t = 50,
    pad = 4
  )
  
  # create function for tool tip, since it will be used in all plots
  # easiest to create an empty function instead of object since objects are used in the funtion
  # will need to use different tooltipes for binomial and continuous
  if (binomial == FALSE) {
    
    tool_tip <- function() {
      ~paste0("Geography: ", geo_description,
              "<br>Year:  ", year,
              "<br>Demographic:  ", subtype,
              "<br>Estimate:  ", round( estimate, 2),
              "<br>MOE:  ", round( moe, 2),
              "<br>SE:  ", round( se, 2),
              "<br>CV:  ", round( cv, 2))
      }
    } else {
      
      tool_tip <- function() {
        ~paste0("Geography: ", geo_description,
                "<br>Year:  ", year,
                "<br>Demographic:  ", subtype,
                "<br>Estimate:  ", round( estimate, 3),
                "<br>Lower CI:  ", round( Lower, 3),
                "<br>Upper CI:  ", round( Upper, 3))  
      }
    }
  
  # create line graph
  if (plot_type == 'line') {
    
    df %>%
      # if the demographic is not comparison community then we only want the selected geographic area
      filter(geo_description == if (input_type != 'Comparison Community') input_geo else .$geo_description) %>%
      plot_ly(x = ~year, y = ~estimate, 
              color = if (input_type == 'Comparison Community') ~geo_description else ~subtype, 
              mode = 'lines', type = 'scatter',
              # tooltip info
              hoverinfo = 'text',
              text = tool_tip()) %>%
      add_ribbons(ymin = if (binomial==F) ~estimate - moe else ~Lower,
                  ymax = if (binomial==F) ~estimate + moe else ~Upper,
                  alpha = 0.15,
                  line = list(width = 0, dash = 'dot'),
                  showlegend = FALSE,
                  hoverinfo = 'skip') %>%
      layout(title = paste0('Yearly Change By ', input_type),
             margin = m)
    
  } else if (plot_type == 'bar') {
    
    # Bar chart
    
    df %>%
      # only keep most current year
      filter(year == recent_year) %>%
      plot_ly(x = ~geo_description, y = ~estimate, color = ~subtype, 
              type = 'bar',
              # error_y = ~list(#type = 'data',
              #                 array = se,
              #                 color = '#000000'),
              # tooltip info
              hoverinfo = 'text',
              text = tool_tip()) %>%
      layout(title = paste0(input_type, ' Differences By Comparison Communities in ', recent_year),
             xaxis = list(title = 'Geographic Unit'),
             margin = m)
    
  }
}

create_datasets <- function(df_list, data_input) {
  
  df_list$data <- read_csv(data_input) 
  
  # add moe and CV if there is an SE column in dataframe
  if ("se" %in% colnames(df_list$data)) {
  df_list$data <- df_list$data %>% 
    # add MOE and CV
    mutate(moe = round(se*1.96, 2),
           cv = round((estimate/se)*100, 2)) %>%
    # round estiamte and SE after calcualting moe and cv so that
    # moe and cv are based on non-rounded values
    mutate(estimate = round(estimate, 2),
           se = round(se, 3))
  }
  
  # store unique values for drop-down lists and filtering
  df_list$demographics <- unique(df_list$data$type)
  df_list$year <- unique(df_list$data$year)
  df_list$geographies <- unique(df_list$data$geo_description)
  
  return(df_list)

}

ff_data_dt <- function(df, trials = FALSE) {
  
  # Input:
  #   df: a dataframe of raw data that you want convert to a DT datatable
  #   col_names: column names for the datatable
  #   trials: whether the data contains columns of successes and trials
  #
  # To color cv values, the cv column must be names 'cv'
  #
  # Note: Do not use this to create a table of z-scores; use ff_acs_zscore_dt
  
  # ensure columns are in proper order and change name of columns
  df <- df %>%
    select(year, geo_description, type, subtype, estimate, everything()) %>%
    rename(geography = geo_description, demographic = type, subdemographic = subtype)
    
  datatable(df,
            filter='top', extensions='Buttons', rownames = FALSE,
            #colnames = col_names,
            options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip'))
}