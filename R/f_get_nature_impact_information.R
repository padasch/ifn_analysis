get_nature_impact_information <- function(df_input) {
  
  # Variable Overview: https://docs.google.com/spreadsheets/d/1MD-fXwS8ZCcWsOjwcMYLyM1Qef-T6uwf0OKVQjJjqJg/edit#gid=0
  
  # Select environmental assessment variables
  env_vars <- get_vars("nature")

  # Set all NA to 9999 to make logical comparisons easier
  # Note: Since some variables were encoded as factors, and because R messes up
  # numbers going from factor to numeric, we have to take the detour via characters
  df_tmp <- 
    df_input |> 
    select(tree_id, all_of(env_vars)) |> 
    mutate(across(all_of(env_vars), as.character)) |> 
    mutate(across(all_of(env_vars), as.numeric)) |> 
    mutate_all(~ifelse(is.na(.), 9999, .))
  
  # V1 ----
  # Assessment of natural effects at first visit
  # High-level influence indicator
  v1_l1_nature      <- 9999
  
  # Medium-level influence incidcators
  v1_l2_abiotic     <- 9999
  v1_l2_biotic      <- 9999
  v1_l2_mortality   <- 9999
  
  # Low-level influence indicators
  # Abiotic Impacts
  v1_lvl3_storm     <- 9999 
  v1_lvl3_landslide <- 9999     
  v1_lvl3_fire      <- 9999
  v1_lvl3_frost     <- 9999
  v1_lvl3_otherabio <- 9999
  
  # Biotic Impacts
  v1_lvl3_mistle    <- 9999 
  v1_lvl3_firrust   <- 9999 
  v1_lvl3_moth      <- 9999 
  v1_lvl3_game      <- 9999
  
  # Signs of mortality
  v1_lvl3_rot               <- 9999
  v1_lvl3_deadbranch        <- 9999
  v1_lvl3_generalmortality  <- 9999
  
  # Information on mode of death
  v1_mode_of_death <- 9999
  
  ## Abiotic ----
  ### NINCID v1 ----
  # Presence of multiple indicators
  # if !identical((df_tmp$nincid_1, 9999)) {
  if (!identical(df_tmp$nincid_1, 9999)) {
      if (identical(df_tmp$nincid_1, 1)) {v1_lvl3_fire      <- 1}
      if (identical(df_tmp$nincid_1, 2)) {v1_lvl3_generalmortality <- 1}
      if (identical(df_tmp$nincid_1, 3)) {v1_lvl3_landslide <- 1}
      if (identical(df_tmp$nincid_1, 4)) {v1_lvl3_storm     <- 1}
      if (identical(df_tmp$nincid_1, 5)) {v1_lvl3_otherabio <- 1}
  } 
  
  ## Biotic ----
  ### SFGELIV v1 ----
  # Presence of frost damage
  if (!identical(df_tmp$sfgeliv_1, 9999)) {
    if (identical(df_tmp$sfgeliv_1, 0)) {
      v1_lvl3_frost <- 0
    } else {
      v1_lvl3_frost <- 1
    }
  }
  
  ### SFGUI v1 ----
  # Presence of significant amount of mistletoe
  if (!identical(df_tmp$sfgui_1, 9999)) {
    if (df_tmp$sfgui_1 %in% c(0, 1)) {
      v1_lvl3_mistle <- 0
    } else {
      v1_lvl3_mistle <- 1
    }
  }
  
  ### SFDORGE v1 ----
  # Presence of fir rust
  if (!identical(df_tmp$sfdorge_1, 9999)) {
    if (identical(df_tmp$sfdorge_1, 0)) {
      v1_lvl3_firrust <- 0
    } else {
      v1_lvl3_firrust <- 1
    }
  }
  
  ### DPYR ----
  # Presence of moths
  if (!identical(df_tmp$dpyr, 9999)) {
    if (identical(df_tmp$dpyr, 0)) {
      v1_lvl3_moth <- 0
    } else {
      v1_lvl3_moth <- 1
      }
  }
  
  ### DEGGIB ----
  # Presence of game damage
  if (!identical(df_tmp$deggib, 9999)) {
    if (df_tmp$deggib %in% c(0, 2)) {
      v1_lvl3_game <- 0
    } else {
      v1_lvl3_game <- 1
    }
  }
  
  ## Tree State ----
  # Evaluate whether there are signs for tree mortality
  
  ### MORTB v1 ----
  # Indicator of branch mortality
  if (!identical(df_tmp$mortb_1, 9999)) {
    if (identical(df_tmp$mortb_1, 0)) {
      v1_lvl3_deadbranch <- 0
    } else {
      v1_lvl3_deadbranch <- 1
    }
  }
  
  ### SFCOEUR & SFPIED----
  # SFCOEUR: Indicator for heart rot
  # SFPIED:  Indicator for foot rot
  rot_heart <- 9999
  rot_foot  <- 9999
  
  if (!identical(df_tmp$sfcoeur, 9999)) {
    if (identical(df_tmp$sfcoeur, 0)) {
      rot_heart <- 0
    } else {
      rot_hear <- 1
    }
  }
  
  if (!identical(df_tmp$sfpied, 9999)) {
    if (identical(df_tmp$sfpied, 0)) {
      rot_foot <- 0
    } else {
      rot_foot <- 1
    }
  }
  
  if (1 %in% c(rot_heart, rot_foot)) {
    v1_lvl3_rot <- 1
  } else if (0 %in% c(rot_heart, rot_foot)) {
    v1_lvl3_rot <- 0
  }
  
  ### ACCI ----
  # Classification of accident type (mostly information on tree state and mod)
  if (!identical(df_tmp$acci, 9999)) {
    if (df_tmp$acci %in% c(1, 3)) {v1_mode_of_death <- "fallen"}
    if (identical(df_tmp$acci, 4)) {v1_lvl3_fire <- 1}
  }
  
  ## Final assessment ----
  # Approach: In most cases, there will be only NA values on the third level.
  # So, if there is one 1 or one 0 value recorded, that will be propagated to
  # the upper level assessments because that is all we have.
  
  ### Level 3 to 2 Aggregation ----
  #### Abiotic ----
  # The `nincid` variable is a location-level assessment if the trees have
  # been influence by any incident. The `acci` variable is a tree-level assess-
  # ment of any accidents. If either of these variables is a 0, the tree
  # or location has not been significantly influenced by any nature-related 
  # influence.
  
  if (identical(df_tmp$nincid_1, 0) | identical(df_tmp$incid_1, 0) | identical(df_tmp$acci, 0)) {
    
    v1_l2_abiotic <- 0
    
  } else if (1 %in% c(v1_lvl3_storm,
                      v1_lvl3_landslide,
                      v1_lvl3_fire,
                      v1_lvl3_frost,
                      v1_lvl3_otherabio)) {
    
    v1_l2_abiotic <- 1
    
  } else if (0 %in% c(v1_lvl3_storm,
                      v1_lvl3_landslide,
                      v1_lvl3_fire,
                      v1_lvl3_frost,
                      v1_lvl3_otherabio)) {
    
    v1_l2_abiotic <- 0
  }
  
  #### Biotic ----
  if (1 %in% c(v1_lvl3_mistle,
               v1_lvl3_firrust,
               v1_lvl3_moth,
               v1_lvl3_game
               )) {
    
    v1_l2_biotic <- 1
    
  } else if (0 %in% c(v1_lvl3_mistle,
                      v1_lvl3_firrust,
                      v1_lvl3_moth,
                      v1_lvl3_game)) {
    
    v1_l2_biotic <- 0
  }
  
  #### Mortality Indications ----
  if (1 %in% c(v1_lvl3_rot,        
               v1_lvl3_deadbranch,
               v1_lvl3_generalmortality)) {
    
    v1_l2_mortality <- 1
    
  } else if (0 %in% c(v1_lvl3_rot,        
                      v1_lvl3_deadbranch,
                      v1_lvl3_generalmortality)) {
    
    v1_l2_mortality <- 0
  }
  
  ### Level 2 to 1 Aggregation ----
  if (1 %in% c(v1_l2_abiotic,        
               v1_l2_biotic,
               v1_l2_mortality)) {
    
    v1_l1_nature <- 1
    
  } else if (0 %in% c(v1_l2_abiotic,        
                      v1_l2_biotic,
                      v1_l2_mortality)) {
    
    v1_l1_nature <- 0
    
  }
  
  v1_nature_impact <- 
    tibble(
      v1_l1_nature,
      v1_l2_abiotic,
      v1_l2_biotic,
      v1_l2_mortality,
      v1_lvl3_storm, 
      v1_lvl3_landslide,     
      v1_lvl3_fire,
      v1_lvl3_frost,
      v1_lvl3_otherabio,
      v1_lvl3_mistle, 
      v1_lvl3_firrust, 
      v1_lvl3_moth, 
      v1_lvl3_game,
      v1_lvl3_rot,
      v1_lvl3_deadbranch,
      v1_lvl3_generalmortality,
      v1_mode_of_death
    )
  
  # V2 -------------------------------------------------------------------------
  
  # Assessment of natural effects at first visit
  # High-level influence indicator
  v2_l1_nature      <- 9999
  
  # Medium-level influence indicators
  v2_l2_abiotic     <- 9999
  v2_l2_biotic      <- 9999
  v2_l2_mortality   <- 9999
  
  # Low-level influence indicators
  # Abiotic Impacts
  v2_lvl3_storm     <- 9999 
  v2_lvl3_landslide <- 9999     
  v2_lvl3_fire      <- 9999
  v2_lvl3_frost     <- 9999
  v2_lvl3_otherabio <- 9999
  
  # Biotic Impacts
  v2_lvl3_mistle    <- 9999 
  v2_lvl3_firrust   <- 9999
  # v2_lvl3_moth      <- 9999 
  # v2_lvl3_game      <- 9999
  
  # Signs of mortality
  # v2_lvl3_rot               <- 9999
  v2_lvl3_deadbranch        <- 9999
  v2_lvl3_generalmortality  <- 9999
  
  # Information on mode of death
  # v2_mode_of_death <- 9999
  
  ## Abiotic ----
  ### NINCID v2 ----
  # Presence of multiple indicators
  if (!identical(df_tmp$nincid_2, 9999)) {
    if (identical(df_tmp$nincid_2, 1)) {v2_lvl3_fire      <- 1}
    if (identical(df_tmp$nincid_2, 2)) {v2_lvl3_generalmortality <- 1}
    if (identical(df_tmp$nincid_2, 3)) {v2_lvl3_landslide <- 1}
    if (identical(df_tmp$nincid_2, 4)) {v2_lvl3_storm     <- 1}
    if (identical(df_tmp$nincid_2, 5)) {v2_lvl3_otherabio <- 1}
  } 
  
  ## Biotic ----
  ### SFGELIV v2 ----
  # Presence of frost damage
  if (!identical(df_tmp$sfgeliv_2, 9999)) {
    if (identical(df_tmp$sfgeliv_2, 0)) {
      v2_lvl3_frost <- 0
    } else {
      v2_lvl3_frost <- 1
    }
  }
  
  ### SFGUI v2 ----
  # Presence of significant amount of mistletoe
  if (!identical(df_tmp$sfgui_2, 9999)) {
    if (df_tmp$sfgui_2 %in% c(0, 1)) {
      v2_lvl3_mistle <- 0
    } else {
      v2_lvl3_mistle <- 1
    }
  }
  
  ### SFDORGE v2 ----
  # Presence of fir rust
  if (!identical(df_tmp$sfdorge_2, 9999)) {
    if (identical(df_tmp$sfdorge_2, 0)) {
      v2_lvl3_firrust <- 0
    } else {
      v2_lvl3_firrust <- 1
    }
  }
  
  ## Tree State ----
  # Evaluate whether there are signs for tree mortality
  
  ### MORTB v2 ----
  # Indicator of branch mortality
  if (!identical(df_tmp$mortb_2, 9999)) {
    if (identical(df_tmp$mortb_2, 0)) {
      v2_lvl3_deadbranch <- 0
    } else {
      v2_lvl3_deadbranch <- 1
    }
  }
  
  ## Final assessment ----
  # Approach: In most cases, there will be only NA values on the third level.
  # So, if there is one 1 or one 0 value recorded, that will be propagated to
  # the upper level assessments because that is all we have.
  
  ### Level 3 to 2 Aggregation ----
  # The `nincid` variable is a location-level assessment if the trees have
  # been influence by any incident. The `acci` variable is a tree-level assess-
  # ment of any accidents. If either of these variables is a 0, the tree
  # or location has not been significantly influenced by any nature-related 
  # influence.
  
  if (identical(df_tmp$nincid_2, 0) | identical(df_tmp$incid_2 , 0)) {
    
    v2_l2_abiotic <- 0
    
  } else if (1 %in% c(v2_lvl3_storm,
                      v2_lvl3_landslide,
                      v2_lvl3_fire,
                      v2_lvl3_frost,
                      v2_lvl3_otherabio)) {
    
    v2_l2_abiotic <- 1
    
  } else if (0 %in% c(v2_lvl3_storm,
                      v2_lvl3_landslide,
                      v2_lvl3_fire,
                      v2_lvl3_frost,
                      v2_lvl3_otherabio)) {
    
    v2_l2_abiotic <- 0
  }
  
  #### Biotic ----
  if (1 %in% c(v2_lvl3_firrust)) {
    
    v2_l2_biotic <- 1
    
  } else if (0 %in% c(v2_lvl3_firrust)) {
    
    v2_l2_biotic <- 0
  }
  
  #### Mortality Indications ----
  if (1 %in% c(v2_lvl3_deadbranch,
               v2_lvl3_generalmortality)) {
    
    v2_l2_mortality <- 1
    
  } else if (0 %in% c(v2_lvl3_deadbranch,
                      v2_lvl3_generalmortality)) {
    
    v2_l2_mortality <- 0
  }
  
  ### Level 2 to 1 Aggregation ----
  if (1 %in% c(v2_l2_abiotic,        
               v2_l2_biotic,
               v2_l2_mortality)) {
    
    v2_l1_nature <- 1
    
  } else if (0 %in% c(v2_l2_abiotic,        
                      v2_l2_biotic,
                      v2_l2_mortality)) {
    
    v2_l1_nature <- 0
    
  }
  
  v2_nature_impact <- 
    tibble(
      v2_l1_nature,
      v2_l2_abiotic,
      v2_l2_biotic,
      v2_l2_mortality,
      v2_lvl3_storm, 
      v2_lvl3_landslide,     
      v2_lvl3_fire,
      v2_lvl3_frost,
      v2_lvl3_otherabio,
      v2_lvl3_mistle, 
      v2_lvl3_firrust, 
      # v2_lvl3_moth, 
      # v2_lvl3_game,
      # v2_lvl3_rot,
      v2_lvl3_deadbranch,
      v2_lvl3_generalmortality
      # v2_mode_of_death
    )
  
  # Replace 9999 again with NA
  v1_nature_impact[(v1_nature_impact == 9999)] <- NA
  v2_nature_impact[(v2_nature_impact == 9999)] <- NA
  
  # Combine information from both visits into one df
  out <- 
    bind_cols(tree_id = as.factor(df_tmp$tree_id),
              v1_nature_impact |> mutate_if(is.double, as.factor), 
              v2_nature_impact |> mutate_if(is.double, as.factor))
  
  # Return
  return(out)
}
