get_ggplot_labs <- function(target_var = NULL, return_available_vars = FALSE){
  
  # ______________________________________________________________________________
  available_vars <- get_available_metrics_of_change()
 
  if (return_available_vars) {
    return(available_vars)
  }
  
  if (is.null(target_var)) {
    stop("Available target variables are: \n", paste0(available_vars, collapse = "\n - "))
  }

  if (!(target_var) %in% available_vars) {
    stop("\nUnsupported input! Available target variables are:\n - ", paste0(available_vars, collapse = "\n - "))
  }
  
  # ______________________________________________________________________________
  l_out <- list()
  
  # ______________________________________________________________________________
  # Based on Hoshino et al 2002
  # Stem-based mortality: n_mor_yr
  if (target_var == "n_mor_yr") {
    my_title   <- "Natural Mortality (stem-based)"
    my_caption <- "Following Hoshino et al. 2002:  m = log(N_initial / N_survivors) / 5 * 100"
    my_axis    <- expression(paste("Mortality [%-stems ", yr ^ -1, "]"))
    my_colors  <- "Reds"
    
  # Stem-based recruitment: n_rec_yr
  } else if (target_var == "n_rec_yr") {
    my_title   <- "Recruitment (stem-based)"
    my_caption <- "Recruitment based on Hoshino et al. 2002: r = log(N_alive_2nd_visit / N_survivors) / 5 * 100"
    my_axis    <- expression(paste("Recruitment [%-stems ", yr ^ -1, "]"))
    my_colors  <- "Greens"
    
  # BA-based mortality: ba_loss_yr
  } else if (target_var == "ba_loss_yr") {
    my_title   <- "Natural Mortality (BA-based)"
    my_caption <- "Following Hoshino et al. 2002: m = log([BA at 1st visit of alive trees] / [BA at 1st visit of survivors]) / 5 * 100 "
    my_axis    <- expression(paste("Mortality [%-BA ", yr ^ -1, "]"))
    my_colors  <- "Reds"
    
  # BA-based ingrowth: ba_ingr_yr
  } else if (target_var == "ba_ingr_yr") {
    my_title   <- "Ingrowth (BA-based)"
    my_caption <- "Following Hoshino et al. 2002: i = log([BA at 2nd visit of survivors trees] / [BA at 1st visit of survivors]) / 5 * 100"
    my_axis    <- expression(paste("Ingrowth [%-BA ", yr ^ -1, "]"))
    my_colors  <- "Greens"
    
  # BA-based gainL ba_gain_yr
  } else if (target_var == "ba_gain_yr") {
    my_title   <- "Gain (BA-based)"
    my_caption <- "Following Hoshino et al. 2002: g = log([BA at 2nd visit of alive trees] / [BA at 2nd visit of survivors]) / 5 * 100"
    my_axis    <- expression(paste("Gain [%-BA ", yr ^ -1, "]"))
    my_colors  <- "Greens"
    
  # ______________________________________________________________________________
  # Esquivel-Mueller et al 2020
  # n_mor_yr_esq
  } else if (target_var == "n_mor_yr_esq") {
    my_title   <- "Natural Mortality (stem-based)"
    my_caption <- "Following Esquivel et al. 2020:  m = (1 − (N_survivors / N_initial)^(1/t)) × 100"
    my_axis    <- expression(paste("Mortality [%-stems ", yr ^ -1, "]"))
    my_colors  <- "Reds"
    
  # ______________________________________________________________________________
  # My definitions
  # ba_loss_abs
  } else if (target_var == "ba_loss_abs") {
    my_title   <- "Natural Mortality (absolute loss of basal area)"
    my_caption <- "m = ba_at_v2_of_dead / 5"
      # "If aggregated over region, change was normalized by number of plots per region"
    my_axis    <- expression(paste("Mortality [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    my_colors  <- "Reds"
    
  # ba_loss_rate
  } else if (target_var == "ba_loss_rate") {
    my_title   <- "Natural Mortality (relative loss of basal area)"
    my_caption <- "m = ba_at_v2_of_dead / (ba_at_v2_of_dead + ba_at_v2_of_survivors) / 5"
    my_axis    <- expression(paste("Mortality [%-BA", yr ^ -1, "]"))
    # my_axis    <- expression(paste("Mortality [% ", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    my_colors  <- "Reds"
    
  # ba_growth_abs
  } else if (target_var == "ba_growth_abs") {
    my_title   <- "Growth (absolute gain of basal area)"
    my_caption <- "g = (ba_at_v2_of_alive_trees - ba_at_v1_of_surivors) / 5"
    my_axis    <- expression(paste("Growth [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    my_colors  <- "Greens"
      
  # ba_growth_rate
  } else if (target_var == "ba_growth_rate") {
    my_title   <- "Growth (relative gain of basal area)"
    my_caption <- "g = (ba_at_v2_of_alive_trees - ba_at_v1_of_surivors) / (ba_at_v1_of_surivors) / 5"
    my_axis    <- expression(paste("Growth [%-BA", yr ^ -1, "]"))
    # my_axis    <- expression(paste("Growth [% ", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    my_colors  <- "Greens"
  }
  
  l_out$title   <- my_title
  l_out$caption <- my_caption
  l_out$axis    <- my_axis
  l_out$colors  <- my_colors
  
  return(l_out)
  
}
