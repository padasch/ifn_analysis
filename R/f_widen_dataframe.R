widen_dataframe <- function(df_data, df_vars, data_type) {
  
  # Verbose
  message(
    "\nWorking on widening dataframe",
    "\n-------------------------------------------------------------------------"
  )
  
  # Check data type
  if (data_type == "location") {
    base_vars <- c("idp")
    qc_var    <- "idp"
  } else if (data_type == "tree") {
    base_vars <- c("idp", "a", "tree_id")
    qc_var    <- "tree_id"
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
  
  vars_first_sight <-  
    df_vars |>
    filter(sampling_strategy == "at_first_sight") |> 
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
  if (length(vars_1st_visit) == 0) {
    first_visits <-  
      df_data |> 
      select(vars_that_stay) |> 
      distinct()
    
  } else if (data_type == "location") {
    first_visits <- 
      df_data |> 
      filter(visite == 1) |> 
      select(vars_that_stay, vars_1st_visit)
    
  } else if (data_type == "tree") {
    first_visits <- 
      df_data |> 
      filter(
        (visite == 1) |
        (visite == 2 & revisit_state == "newly_sampled")
          ) |> 
      select(vars_that_stay, vars_1st_visit)
  }
  
  # Get variables that are only measured during second visit
  if (length(vars_2nd_visit) == 0) {
    second_visits <-  
      df_data |> 
      select(vars_that_stay) |> 
      distinct()
  } else {
    second_visits <- 
      df_data |> 
      filter(visite == 2) |> 
      select(vars_that_stay, vars_2nd_visit)
  }
  
  # Ger variables that are only measured at first sight
  # (either first OR second visit)
  if (length(vars_first_sight) == 0) {
    first_sight <-  
      df_data |> 
      select(vars_that_stay) |> 
      distinct()
  } else {
    first_sight <-
      df_data |> 
      select(vars_that_stay, vars_first_sight) |> 
      distinct() |> 
      drop_na(vars_first_sight)
  }
  
  # Combine all again
  combined <- 
    widened |> 
    left_join(first_visits,  by = vars_that_stay) |> 
    left_join(second_visits, by = vars_that_stay) |> 
    left_join(first_sight, by = vars_that_stay)
  
  # Quality check for duplicates:
  if (length(unique(combined[[qc_var]])) != nrow(combined)) {stop("QC FAILED!")}
  
  # return data
  return(combined)
}
