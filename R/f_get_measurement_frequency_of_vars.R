get_measurement_frequency_of_vars <- function(input, input_type) {
  
  # Define function used later
  var_measurement_frequency <- function(v1, v2) {
    
    if ( is.na(v1)  & is.na(v2))  {return("not_measured")}
    if (!is.na(v1)  & is.na(v2))  {return("only_1st_visit")}
    if ( is.na(v1)  & !is.na(v2)) {return("only_2nd_visit")}
    if (identical(v1, v2))        {return("same_both_visits")}
    if (!identical(v1, v2) )      {return("changes_between_visits")}
    
  }
  
  # Check input
  if (input_type == "location") {
    vars   <- input |> select(-idp) |> names()
    i_vars   <- length(vars)
    
    df_nested <- 
      input |> 
      group_by(idp) |> 
      mutate(n_visits = n()) |> 
      filter(n_visits > 1) |> 
      nest() |> 
      ungroup()
    
  } else if (input_type == "tree") {
    vars   <- input |> select(-idp, -a) |> names()
    i_vars <- length(vars)
    
    df_nested <- 
      input |> 
      group_by(idp, a) |> 
      mutate(n_visits = n()) |> 
      filter(n_visits > 1) |> 
      nest() |> 
      ungroup()
  } else {
    stop("Invalid Input: ", deparse(substitute(input)))
  }
  
  # Run assessment
  message(
    "\nAssessing variable sampling frequency for:", (deparse(substitute(input))),
    "\n--------------------------------------------------------------------------"
    )
  
  df_fin <- tibble()
  i <- 0
  
  for (var in vars) {
    
    i <- i + 1
    cat("\nWorking on variable:\t", i, "/", i_vars, "\t", var)
    
    # Apply the function to each row of the nested data frames
    df_tmp <- 
      df_nested %>%
      mutate(
        results = map_chr(data, ~ var_measurement_frequency(.[[var]][1], .[[var]][2]))
      )
    
    var_tmp <- 
      df_tmp |> 
      filter(results != "not_measured") |> 
      pull(results) |> 
      unique()
    
    if (length(var_tmp) == 1) {
      
      if (var_tmp == "changes_between_visits") {
        sampling_frequency <- "changes"
        sampling_strategy  <- "changes_between_visits"
      } else {
        sampling_frequency <- "constant"
        sampling_strategy  <- var_tmp
      }
    } else {
      sampling_frequency <- "changes"
      sampling_strategy  <- "changes_between_visits"
    }
    
    i_df <- 
      tibble(var = var,
             sampling_frequency = sampling_frequency,
             sampling_strategy = sampling_strategy
             )
    
    df_fin <- rbind(df_fin, i_df)
  }
  
  df_fin <- df_fin |> arrange(sampling_frequency, sampling_strategy)
  
  return(df_fin)
}
