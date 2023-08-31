widen_dataframe <- function(df_data, df_vars, data_type) {
  
  # Verbose
  message(
    "\nWorking on widening dataframe",
    "\n-------------------------------------------------------------------------"
  )
  
  # Check data type
  if (data_type == "location") {
    base_vars <- c("idp")
  } else if (data_type == "tree") {
    base_vars <- c("idp", "a", "tree_id")
  } else{
    stop("Invalid data_type:", data_type)
  }
  
  # Define variables for access later
  vars_that_stay <- 
    df_vars |>
    filter(sampling_strategy == "same_both_visits") |> 
    pull(var)
  
  vars_that_stay <- c(base_vars, vars_that_stay) |> unique()
  
  vars_that_change <- 
    df_vars |>
    filter(sampling_strategy == "changes_between_visits") |> 
    pull(var)
  
  vars_1st_visit <- 
    df_vars |>
    filter(sampling_strategy == "only_1st_visit") |> 
    pull(var)
  
  vars_2nd_visit <- 
    df_vars |>
    filter(sampling_strategy == "only_2nd_visit") |> 
    pull(var)
  
  # Wide variables that are measured twice
  widened <- 
    df_data |> 
    select(all_of(vars_that_stay), vars_that_change) |>
    pivot_wider(
      id_cols     = vars_that_stay,
      names_from  = visite,
      values_from = vars_that_change
      )
    
  # Get variables that are only measured during first visit
  if (data_type == "location") {
    first_visits <- 
      df_data |> 
      filter(visite == 1) |> 
      select(vars_that_stay, vars_1st_visit)
  } else {
    first_visits <- 
      df_data |> 
      filter(
        (visite == 1) |
        (visite == 2 & revisit_state == "newly_sampled")
          ) |> 
      select(vars_that_stay, vars_1st_visit)
  }
  
  # Get variables that are only measured during first visit
  second_visits <- 
    df_data |> 
    filter(visite == 2) |> 
    select(vars_that_stay, vars_2nd_visit)
  
  # Combine all again
  combined <- 
    widened |> 
    left_join(first_visits,  by = vars_that_stay) |> 
    left_join(second_visits, by = vars_that_stay) 
  
  # return data
    
  return(combined)
}
