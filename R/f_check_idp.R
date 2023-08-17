check_idp <- function(n, verbose = TRUE){
  n <- n
  horizonal_line <- paste0(rep("-", 60), collapse = "")
  
  if(verbose) cat(horizonal_line, " DATA FOR IDP: ", n, horizonal_line, "\n")
  
  # l_raw_data$arbre    |> filter(idp == n) |> print(n = 50)
  # l_raw_data$placette |> filter(idp == n) |> print(n = 50)
  
  return(list(arbre = l_raw_data$arbre    |> filter(idp == n),
              loc   = l_raw_data$placette |> filter(idp == n)))
}
