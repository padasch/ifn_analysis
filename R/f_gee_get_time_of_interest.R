gee_get_time_of_interest <- function(
    first_year = "2009",
    last_year  = "2022",
    which_toi){
  
  if (which_toi == "3months") {
    toi <- seq(as.Date(paste0(first_year, "-12-01")), 
               as.Date(paste0(last_year,  "-12-01")),
               by = "3 months")
    toi <- format(toi, "%Y-%m-%d")
    
  } else if (which_toi == "6months") {
    toi <- seq(as.Date(paste0(first_year, "-09-01")), 
               as.Date(paste0(last_year,  "-03-01")),
               by = "6 months")
    toi <- format(toi, "%Y-%m-%d")
    
  } else if (which_toi == "daily") {
    toi <- seq(as.Date(paste0(first_year, "-12-01")), 
               as.Date(paste0(last_year,  "-12-31")),
               by = "1 day")
    toi <- format(toi, "%Y-%m-%d")
    
  } else if (which_toi == "yearly") {
    toi <- seq(as.Date(paste0(first_year, "-01-01")), 
               as.Date(paste0(last_year,  "-12-01")),
               by = "1 year")
    toi <- format(toi, "%Y-%m-%d")
    
  } else {
    stop("Invalid toi.")
  }
  
  return(toi)
  
}
