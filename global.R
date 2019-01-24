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

# z_score_table <- function(list_data, df, demo) {
#   
#   # function that creates z scores
#   
#   
#   # create dataset to be used for z scores
#   zscore_df <- list_data[[df]] %>%
#     # only keep selected demographic
#     filter(type == demo)
#   
#   ff_acs_zscore(zscore_df, 'estimate', 'se', 
#                 c('geo_description', 'year', 'subtype'))
#   
# }
# 
# ff_acs_zscore <- function(data_frame, estimate, se, test, 
#                           success = NULL, trials = NULL, var_names = NULL) {
#   
#   # This function returns a square symetrical matrix of of all significance tests for all combinations of values
#   # The function can calculate either a z-score or a p-value from a Chi-Square test
#   # The matrix length and with equal the number of rows in the data frame
#   #
#   # The z-score formula comes from:
#   #    U.S. Census Bureau, A Compass for Understanding and Using ACS Survey Data, A-18
#   #
#   # Parameters:
#   #   data_frame: the dataframe where the estimates and se are housed
#   #   estimate: a string that is the column name of the column containing the estimate
#   #   se: a string that is the column name of the column containing the se
#   #   var_names: a character vector of variables that can be combined to created
#   #              distinct names for each row and column
#   #   test: either 'z' or 'chi-square'
#   #   success: if chi-square, number of successes
#   #   trials: if chi-square, number of trials
#   
#   
#   # initialize an empty data frame with one column and the same number
#   # of rows as the final dataframe
#   z_score_mat <- data.frame(n = seq(1, nrow(data_frame)))
#   
#   if (test == 'zscore') {
#     
#     # iterate through each row in the dataframe
#     for (i in 1:nrow(data_frame)) {
#       
#       # calculate the point estimate differences and the sum of
#       # of standard errors for the given row and all other rows
#       # this will return a vector
#       estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
#       se_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 )
#       
#       # calculate the z score for all row values, rounds to two decimals
#       z_score <- abs( estimate_diff / se_diff) %>% round(2)
#       
#       # add the row of z scores to the z score matrix
#       z_score_mat[, i] <- z_score
#       
#     }
#     
#   } else if (test == 'chisquare') {
#     
#     # make sure there are columns called 'success' and 'trials'
#     if (!(('success' %in% colnames(data_frame)) & ('trials' %in% colnames(data_frame)))) {
#       
#       stop("The 'success' and 'trials' columns are missing.")
#       
#     }
#     
#     # create vectors of counts and totals, 
#     # leads to shorter code than refering to column names
#     success_c <- data_frame[[success]]
#     trials_c <- data_frame[[trials]]
#     
#     # iterate through each row in the dataframe
#     for (i in 1:nrow(data_frame)) {
#       
#       # conduct proportion test for between value at row in loop and all other valyes
#       p_value <- sapply(1:nrow(data_frame), 
#                         function(x) prop.test(c(success_c[i],success_c[x]),c(trials_c[i],trials_c[x]))$p.value)
#       
#       # add the row of z scores to the z score matrix
#       z_score_mat[, i] <- round(p_value, 3)
#       
#     } 
#     
#   } else {
#     
#     stop("Test must be either 'z' or 'chi-square'")
#     
#   }
#   
#   if (!is.null(var_names)) {
#     
#     # if there is only one variable name, then use this as the label
#     # otherwise paste together variable names
#     if (length(var_names) == 1) {
#       
#       # sometime isolating a column returns a data frame, and sometimes it returns a vector
#       # if a dataframe is returned, isolate first, and only, column as a vector
#       if (is.data.frame(unique(data_frame[ , var_names])) == TRUE) {
#         
#         names_vec <- unique(data_frame[ , var_names])[[1]]
#         
#       } else {
#         
#         names_vec <- unique(data_frame[ , var_names])
#         
#       }
#       
#     } else {
#       
#       # create vector of label names by pasting columns together
#       names_vec <- apply( data_frame[ , var_names], 1, paste, collapse = ": " )
#       
#     }
#     
#     # shorted names so they appear cleaner and shorter in the matrix as column and row headers
#     
#     # replace any United States and North Carolina values with NC and US
#     names_vec <- str_replace_all(names_vec, 'United States', 'US') %>%
#       str_replace_all('North Carolina', 'NC') %>%
#       str_replace_all(' County, NC', '') %>%
#       # replace and ethnicities with abbreviation
#       str_replace_all('African American', 'AA') %>%
#       str_replace_all('Hispanic/Latino', 'HL') %>%
#       str_replace_all('White, non-Hispanic', 'Wh') %>%
#       # shorten age descriptions (take off the word 'year')
#       str_replace_all(' years', '') %>%
#       str_replace_all(' and over', '+') %>%
#       # shorten age by converting 'to' to '-'
#       str_replace_all(' to ', '-') %>%
#       # remove word 'ratio;
#       str_replace_all(' ratio', '')
#     
#     # add labels as column and row names
#     colnames(z_score_mat) <- names_vec
#     row.names(z_score_mat) <- names_vec
#     
#   }
#   
#   return(z_score_mat)
#   
# }

# ff_acs_zscore_kable <- function(data_frame, estimate, se, test,
#                                 success = NULL, trials = NULL, var_names = NULL, 
#                                 table_name = 'Significance test') {
#   
#   # This function takes as input a matrix of z score generated by ff_acs_zscore
#   # it returns a kable table of z scores with scores over 1.96 in bold
#   
#   # input:
#   #   zscore_matrix: matrix of z-scores generated from ff_acs_zscore
#   #   table_name: table caption name for kable table
#   
#   # for z-score we want to bold anything over 1.96,
#   # for chi-square, we want to bold anything under 0.05
#   # each of these values represent the significance threshold
#   thresh <- if (test == 'z') 1.96 else 0.05
#   # we want to bold numbers over threshold for z and under threshold for chi-square
#   # due to this difference, we must create TRUE and FALSE values of whether to bold
#   # depending on what test is used
#   if_true_bold <- if (test =='z') T else F
#   if_false_bold <- if (test =='z') F else T
#   
#   data_frame %>%
#     ff_acs_zscore(estimate, se, test = test, success = success, trials = trials, var_names = var_names) %>%
#     # bold any z score over 1.96
#     mutate_all(funs(cell_spec(., 
#                               bold = ifelse(. > thresh, 
#                                             if_true_bold,
#                                             if_false_bold)))) %>%
#     # add column names as the first row because row names do not print
#     mutate(Compare = colnames(.),
#            # bold column of column / row names
#            Compare = cell_spec(Compare, bold = T)) %>%
#     # only keep rows of Forsyth County
#     filter(str_detect(Compare, 'Forsyth')) %>%
#     # make the comparison column (column and row names) the first column
#     select(Compare, everything()) %>%
#     # create kable table
#     kable(caption = table_name, escape = F)  %>%
#     # add formating (every other row in gray)
#     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left", font_size = 10) %>%
#     # bold row names
#     column_spec(1, bold = T)
# }

# estimates_ci <- function(data_frame, estimate, se, test,
#                          success = NULL, trials = NULL, var_names = NULL) {
#   
#   # This function takes as input a dataframe with estimates and standard errors,
#   # or successes and trials; and calcualted the estimated difference between two
#   # point estimates as well as 95% CIs
#   
#   # initialize an empty data frame with one column and the same number
#   # of rows as the final dataframe
#   estimate_mat <- data.frame(n = seq(1, nrow(data_frame)))
#   
#   if (test == 'zscore') {
#     
#     # iterate through each row in the dataframe
#     for (i in 1:nrow(data_frame)) {
#       
#       # calculate the point estimate differences and the moe 
#       # for the given row and all other rows this will return a vector
#       # must convert to character so estimates and CI can be pasted in single cell
#       estimate_diff <- data_frame[[i, estimate]] - data_frame[[estimate]]
#       moe_diff <- sqrt( data_frame[[i, se]]^2 + data_frame[[se]]^2 ) * 1.96
#       
#       # create single cell that has estimate and CIs
#       cell_values <- sprintf("%.2f,\n[%.2f, %.2f]", estimate_diff, estimate_diff-moe_diff, estimate_diff+moe_diff)
#       
#       # add the row of z scores to the z score matrix
#       estimate_mat[, i] <- cell_values
#       
#     }
#     
#   }
#   
#   if (!is.null(var_names)) {
#     
#     # if there is only one variable name, then use this as the label
#     # otherwise paste together variable names
#     if (length(var_names) == 1) {
#       
#       # sometime isolating a column returns a data frame, and sometimes it returns a vector
#       # if a dataframe is returned, isolate first, and only, column as a vector
#       if (is.data.frame(unique(data_frame[ , var_names])) == TRUE) {
#         
#         names_vec <- unique(data_frame[ , var_names])[[1]]
#         
#       } else {
#         
#         names_vec <- unique(data_frame[ , var_names])
#         
#       }
#       
#     } else {
#       
#       # create vector of label names by pasting columns together
#       names_vec <- apply( data_frame[ , var_names], 1, paste, collapse = ": " )
#       
#     }
#     
#     # shorted names so they appear cleaner and shorter in the matrix as column and row headers
#     
#     # replace any United States and North Carolina values with NC and US
#     names_vec <- str_replace_all(names_vec, 'United States', 'US') %>%
#       str_replace_all('North Carolina', 'NC') %>%
#       str_replace_all(' County, NC', '') %>%
#       # replace and ethnicities with abbreviation
#       str_replace_all('African American', 'AA') %>%
#       str_replace_all('Hispanic/Latino', 'HL') %>%
#       str_replace_all('White, non-Hispanic', 'Wh') %>%
#       # shorten age descriptions (take off the word 'year')
#       str_replace_all(' years', '') %>%
#       str_replace_all(' and over', '+') %>%
#       # shorten age by converting 'to' to '-'
#       str_replace_all(' to ', '-') %>%
#       # remove word 'ratio;
#       str_replace_all(' ratio', '')
#     
#     # add labels as column and row names
#     colnames(estimate_mat) <- names_vec
#     row.names(estimate_mat) <- names_vec
#     
#   }
#   
#   return(estimate_mat)
#   
# }

# estimates_ci_kable <- function(data_frame, estimate, se, test,
#                               success = NULL, trials = NULL, var_names = NULL, 
#                               table_name = 'Estimates and CIs') {
#   
#   # This function takes as input a matrix of estimates generated by estimates_ci
#   # it returns a kable table of estimates
#   
#   data_frame %>%
#     estimates_ci(estimate, se, test, var_names = var_names) %>%
#       #estimate, se, test = test, success = success, trials = trials, var_names = var_names) %>%
#     # add column names as the first row because row names do not print
#     mutate(Compare = colnames(.),
#            # bold column of column / row names
#            Compare = cell_spec(Compare, bold = T)) %>%
#     # only keep rows of Forsyth County
#     filter(str_detect(Compare, 'Forsyth')) %>%
#     # make the comparison column (column and row names) the first column
#     select(Compare, everything()) %>%
#     # create kable table
#     kable(caption = table_name, escape = F)  %>%
#     # add formating (every other row in gray)
#     kable_styling(bootstrap_options = c("striped", "hover"), full_width = F, position = "left", font_size = 10) %>%
#     # bold row names
#     column_spec(1, bold = T)
# }

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