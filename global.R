plotly_plots <- function(df, input_type, plot_type) {
  
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
  tool_tip <- function() {
    ~paste0("Geography: ", geo_description,
            "<br>Year:  ", year,
            "<br>Demographic:  ", subtype,
            "<br>Estimate:  ", round( estimate, 2),
            "<br>MOE:  ", round( moe, 2),
            "<br>SE:  ", round( se, 2),
            "<br>CV:  ", round( cv, 2))
  }
  
  # create line graph
  if (plot_type == 'line') {
    
    df %>%
      # if the demographic is not comparison community then we only want Forsyth County data
      filter(geo_description == if (input_type != 'Comparison Community') 'Forsyth County, NC' else .$geo_description) %>%
      plot_ly(x = ~year, y = ~estimate, 
              color = if (input_type == 'Comparison Community') ~geo_description else ~subtype, 
              mode = 'lines', type = 'scatter',
              # tooltip info
              hoverinfo = 'text',
              text = tool_tip()) %>%
      add_ribbons(ymin = ~estimate - moe,
                  ymax = ~estimate + moe,
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
  
  # if the file is fron google srive, we must use a different process
  # if 'google' is in the URL, do the following
  if (str_detect(data_input, 'google')) {

    # download file to server
    download_file <- drive_download(as_id(data_input), overwrite=TRUE)

    # import file into R
    df_list$data <- read_csv(download_file[[1]]) %>%
      # add MOE and CV
      mutate(moe = round(se*1.96, 2),
             cv = round((estimate/se)*100, 2)) %>%
      # round estiamte and SE after calcualting moe and cv so that
      # moe and cv are based on non-rounded values
      mutate(estimate = round(estimate, 2),
             se = round(se, 3))

    # remove file from hard drive
    file.remove(download_file[[1]])

  } else {
  
  df_list$data <- read_csv(data_input) %>%
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

ff_data_dt <- function(df, for_tableau=FALSE, trials = FALSE) {
  
  # Input:
  #   df: a dataframe of raw data that you want convert to a DT datatable
  #   col_names: column names for the datatable
  #   for_tableau: whether this table is for tableau output
  #   trials: whether the data contains columns of successes and trials
  #
  # To color cv values, the cv column must be names 'cv'
  #
  # Note: Do not use this to create a table of z-scores; use ff_acs_zscore_dt
  
  # ensure columns are in proper order and change name of columns
  df <- df %>%
    select(year, geo_description, type, subtype, estimate, everything()) %>%
    rename(geography = geo_description, demographic = type, subdemographic = subtype)
  
  if (for_tableau == FALSE) {
    
    datatable(df,
              filter='top', extensions='Buttons', rownames = FALSE,
              #colnames = col_names,
              options = list(scrollX = TRUE, scrollY = TRUE, dom = 'Bfrtip')) %>%
      # color cv numbers based on value, only if column named 'cv' exists
      formatStyle('cv', color = styleInterval(c(12, 30), c('black', 'blue', 'red')))
    
  } else {
    
    # if the table is for tableau, we need to add additional rows that represent Forsyth County totals,
    # but change type from Comparison Community to Total
    
    df %>%
      # filter for rows with Forsyth County as the county, and where type starts with Comparison
      filter(str_detect(geography, '^Forsyth'),
             str_detect(demographic, '^Comparison')) %>%
      # change type columns from Comparison to Total
      mutate(demographic = 'Total') %>%
      # bind these rows to the original dataframe
      bind_rows(df) %>%
      # don't need these columns for tableau
      select(-se, -moe, -cv)
    
  }
}