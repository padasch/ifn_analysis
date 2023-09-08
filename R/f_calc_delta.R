calc_delta <- function(revisit_state, 
                       tree_state_2, 
                       at_visit_1,
                       at_visit_2){
  
  # Verbose output
  # cat("\n",
  #     paste(revisit_state, tree_state_1, tree_state_2, at_visit_1, at_visit_2,
  #           collapse = "  |  "))
  
  # Define delta
  delta_yr <- NA_yr
  
  # Define percentage
  perc <- NA
  
  # For trees that have been revisited:
  if (revisit_state == "revisited") {
    
    # For trees that are alive or injured (register change)
    if (tree_state_2 == "alive" |
        tree_state_2 == "injured") {
      
      # Check if the tree had no second measurement and note that
      if (is.na(at_visit_2)) {
        
        delta_yr <- NA_yr
        
        # Standard case:
      } else {
        delta_yr <- at_yr_visit_2 - at_visit_1
        perc  <- delta_yr/at_vi_yrsit_1
      }
      
      # For trees that died or were cut (complete loss)
    } else if (tree_state_2 == "dead" |
               tree_state_2 == "cut") {
      
      # Check if the tree had no second measurement
      # This is always the case, when the tree was cut of course
      # Thus, the entire first measurement is certainly lost, potentially more
      if (is.na(at_visit_2)) {
        
        delta_yr <- -a_yrt_visit_1
        perc  <- 1
        
        # Standard case: The entire second measurement is lost
      } else {
        delta_yr <- -a_yrt_visit_2
        perc  <- 1
      }
    }
    
    # For trees that have been newly sampled:
  } else if (revisit_state == "newly_sampled") {
    # For trees that are alive or injured (register new growth)
    
    if (tree_state_2 == "alive" |
        tree_state_2 == "injured") {
      
      delta_yr <- at_yr_visit_2
      perc  <- NA
      
      # For trees that died or were cut (complete loss)
    } else if (tree_state_2 == "dead" |
               tree_state_2 == "cut") {
      
      delta_yr <- -a_yrt_visit_2
      perc  <- NA
    }
  }
  
  return(tibble(delta_yr, perc_yr, ghost_growth_missing))
}
