f_widen_location_data <- function(df_in) {

  # Get variables that are location specific and need no widening:
  vars_location <- f_get_fixed_vars_per_level(level = "location")
  vars_to_widen <- df_in |> select(-any_of(vars_location)) |>  names()
  
  
  # Widen location data
  df_out <- 
    df_in |> 
    pivot_wider(
      id_cols = vars_location,
      names_from  = visite,
      values_from = vars_to_widen) |> 
    rename_with(~ gsub("_1$", "", .), ends_with("_1")) |> 
    rename_with(~ gsub("_2$", "5", .), ends_with("_2")) |> 
    select(
      -contains("visite"),
      -where(~all(is.na(.)))
    ) 
  
  # Manual renaming
  df_out <- 
    df_out |> 
    rename(prelev5 = prelev55)
  
  return(df_out)
}
