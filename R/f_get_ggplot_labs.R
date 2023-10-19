get_ggplot_labs <- function(target_var = NULL, return_available_vars = FALSE){
  
  # ______________________________________________________________________________
  available_vars <- 
    c(
      "n_mor_yr",
      "n_mor_yr_esq",
      "ba_growth_abs",
      "ba_growth_rate",
      "ba_loss_abs",
      "ba_loss_rate"
    )

  if (return_available_vars) {
    return(available_vars)
  }
  
  if (is.null(target_var)) {
    stop("Available target variables are: \n", paste0(available_vars, collapse = "\n - "))
  }

  if (!(target_var) %in% available_vars) {
    stop("Unsupported input! Available target variables are:\n - ", paste0(available_vars, collapse = "\n - "))
  }
  
  # ______________________________________________________________________________
  l_out <- list()
  
  
  # n_mor_yr
  if (target_var == "n_mor_yr") {
    my_title   <- "Natural Mortality (stem-based)"
    my_caption <- "Mortality based on Hoshino et al. 2002:  m = log(N_initial / N_survivors) / 5 * 100"
    my_axis    <- expression(paste("\nMortality [%-stems ", yr ^ -1, "]"))
    
  # n_mor_yr_esq
  } else if (target_var == "n_mor_yr_esq") {
    my_title   <- "Natural Mortality (stem-based)"
    my_caption <- "Mortality based on Esquivel et al. 2020:  m = (1 − (N_survivors / N_initial)^(1/t)) × 100"
    my_axis    <- expression(paste("\nMortality [%-stems ", yr ^ -1, "]"))
      
  # ba_loss_abs
  } else if (target_var == "ba_loss_abs") {
    my_title   <- "Natural Mortality (absolute loss of basal area)"
    my_caption <- "Mortality = [BA of trees that died between census] / 5"
      # "\nIf aggregated over region, change was normalized by number of plots per region"
    my_axis    <- expression(paste("Mortality [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    
  # ba_loss_rate
  } else if (target_var == "ba_loss_rate") {
    my_title   <- "Natural Mortality (relative loss of basal area)"
    my_caption <- "Mortality = [BA of trees that died between census] / [BA of all alive trees at 1st visit] / 5"
    my_axis    <- expression(paste("Mortality [% ", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    
  # ba_growth_abs
  } else if (target_var == "ba_growth_abs") {
    my_title   <- "Growth (absolute gain of basal area)"
    my_caption <- "Growth = ([BA of survivors at 2nd visit] - [BA of survivors at 1st visit]) / 5"
    my_axis    <- expression(paste("\nGrowth [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
      
  # ba_growth_rate
  } else if (target_var == "ba_growth_rate") {
    my_title   <- "Growth (relative gain of basal area)"
    my_caption <- "Growth = ([BA of survivors at 2nd visit] - [BA of survivors at 1st visit]) / [BA of survivors at 1st visit] / 5"
    my_axis    <- expression(paste("Growth [% ", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
  }
  
  l_out$title   <- my_title
  l_out$caption <- my_caption
  l_out$axis    <- my_axis
  
  return(l_out)
  
}
