format_vars <- function(raw_units,
                        df_in) {
  # Create lists for easy formatting
  l_man <- c()
  l_chr <- c()
  l_num <- c()
  l_fac <- c()
  l_tim <- c()
  
  for (var in names(df_in)) {
    
    check <- var %in% raw_units$variable
    
    if (check) {
      
      # Get class if variable is qualitative or quantitative
      type <- 
        raw_units |> 
        dplyr::filter(var == variable) |> 
        pull(type) |> 
        unique()
      
      if (type == "Qualitatif")  l_fac <- c(l_fac, var)
      if (type == "Quantitatif") l_num <- c(l_num, var)
      if (type == "Temporel")    l_tim <- c(l_tim, var)
      if (type == "Texte")       l_chr <- c(l_chr, var)
      
    } else {
      l_man <- c(l_man, var)  
    }
  }
  
  # Format variables automatically
  df_out <- 
    df_in |>
    mutate(across(l_num, ~ as.double(.)),
           across(l_fac, ~ as.factor(.)),
           across(l_tim, ~ lubridate::as_date(.)),
           across(l_chr, ~ as.character(.))
    ) 
  # Format a few variables by hand
  l_nam <- names(df_out)
  
  if ("CAMPAGNE" %in% l_nam) df_out$CAMPAGNE <- as.double(df_out$CAMPAGNE)
  if ("YL" %in% l_nam)       df_out$YL <- as.double(df_out$YL)
  if ("XL" %in% l_nam)       df_out$XL <- as.double(df_out$XL)
  if ("IDP" %in% l_nam)      df_out$IDP <- as.factor(df_out$IDP)
  if ("A" %in% l_nam)        df_out$A <- as.factor(df_out$A)
  
  # Clean up names
  if ("XL" %in% l_nam) df_out <- df_out |> rename(LON = XL, LAT = YL) # Coords
  df_out <- df_out |> rename_with(str_to_lower)
  
  return(tibble(df_out))
}
