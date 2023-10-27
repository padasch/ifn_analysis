# Functions to create site-level variables ----------------------------------------------------

# Land use classification ---------------------------------------------------------------------
# *PRELEV5:*
#   Indicator for cutting
# -   0 = No trees cut within plot
# -   1 = Some trees cut within plot
# -   2 = All trees cut within plot
# 
# *PEUPNR*
#   Indicator if site was censusable
# -   0 = censusable
# -   1 = non-censusable
# -   2 = temporarily deforested
# 
# *CSA*
#   Ground Cover
# -   1	Closed wooded cover
# -   2	Grove
# -   3	Open wooded cover
# -   4	Heath
# -   5	Poplar grove
# -   6	Other vegetation (cultivated shrublands)
# -   7	Artificialized without vegetation
# -   8	Natural without vegetation (not present in the dataset)
# -   9	Outside territory (not present in the dataset)
# 
# *UTIP*
#   Predominant Land Use
# Variable holds no useful information, most data is NA or "Other"
# -   A = Agriculture
# -   X = Other use
# 
# *UTA1 & UTA2*
#   Primary and Secondary Land Use (similar as UPTIP but more precise)
# -   0 = Wood production
# -   2 = Home, leisure, public park, housing, private park, enclosures
# -   3 = Active military maneuvering ground or prohibited area
# -   4 = Integral reserve with forbidden access
# -   5 = Other reserves
# -   6 = Agricultural
# -   8 = Soil and water protection
# -   9 = No use
# -   A = Network passage
# -   B = Large linear infrastructure right-of-way
# -   X = No use
# 
# *AUTUT*
#   Other Land Use
# -   0 = No use
# -   1 = Military terrain 
# -   2 = Reserve other than full access prohibited
# -   3 = Agricultural
# -   4 = Network passthrough and firewall
# -   5 = Home, leisure
# -   6 = Fenced forest for hunting
# -   A = Agriculture
# -   F = Fire wall
# -   G = Hunting
# -   L = Home and leisure (compatible with wood production)
# -   M = Military
# -   R = Protected areas
# -   T = Network passage (compatible with wood production)
# -   X = ???

classify_utip <- function(input){
  if (input %in% c(NA))  return(NA)
  if (input %in% c("A")) return("Agriculture")
  if (input %in% c("X")) return("Other")
}

classify_autut <- function(input){
  if (input %in% c(NA)) return(NA)
  if (input %in% c("0")) return("No Use")
  if (input %in% c("A", "3")) return("Agriculture")
  if (input %in% c("G", "6")) return("Hunting")
  if (input %in% c("L", "5")) return("Leisure")
  if (input %in% c("M", "1")) return("Military")
  if (input %in% c("R", "2")) return("Protected")
  if (input %in% c("T", "F", "4")) return("Infrastructure")
  if (input %in% c("X")) return(NA)
}

classify_uta <- function(input){
  if (is.na(input)) return(NA)
  if (input %in% c("0")) return("Wood Production")
  if (input %in% c("2")) return("Leisure")
  if (input %in% c("3")) return("Military")
  if (input %in% c("4")) return("Protected")
  if (input %in% c("5")) return("Protected")
  if (input %in% c("6")) return("Agriculture")
  if (input %in% c("8")) return("Protected")
  if (input %in% c("9")) return("No Use")
  if (input %in% c("A")) return("Infrastructure")
  if (input %in% c("B")) return("Infrastructure")
  if (input %in% c("X")) return("No Use")
}

classify_land_use <- function(uta1, uta2, utip_1, utip_2, autut_1, autut_2){
  if (!is.na(uta1)) return(uta1)
  if (!is.na(uta2)) return(uta2)
  if (utip_1 %in% c("Agriculture")) return(utip_1)
  if (utip_2 %in% c("Agriculture")) return(utip_2)
  if (!is.na(autut_1)) return(autut_1)
  if (!is.na(autut_2)) return(autut_2)
  return(NA)
}


# Land use change -----------------------------------------------------------------------------
classify_tree_cover_change <- function(input_1, input_2){
  # Potential changes are: 
  # df_tmp |> mutate(cc = paste0(csa_1, "_", csa_2)) |> pull(cc) |> unique()
  
  if (input_1 %in% c(NA) | input_2 %in% c(NA)) return(NA)
  if (input_1 == input_2) return("No Change")
  
  if (input_1 == 1 & input_2 == 2) return("Decreased Tree Cover")
  if (input_1 == 1 & input_2 == 3) return("Decreased Tree Cover")
  if (input_1 == 1 & input_2 == 4) return("Decreased Tree Cover")
  if (input_1 == 1 & input_2 == 5) return("Decreased Tree Cover")
  if (input_1 == 1 & input_2 == 6) return("Decreased Tree Cover")
  if (input_1 == 1 & input_2 == 7) return("Clearcut")
  if (input_1 == 1 & input_2 == 9) return(NA)
  
  if (input_1 == 2 & input_2 == 1) return("Increased Tree Cover")
  if (input_1 == 2 & input_2 == 3) return("Decreased Tree Cover")
  if (input_1 == 2 & input_2 == 6) return("Decreased Tree Cover")
  if (input_1 == 2 & input_2 == 7) return("Clearcut")
  
  if (input_1 == 3 & input_2 == 1) return("Increased Tree Cover")
  if (input_1 == 3 & input_2 == 6) return("Decreased Tree Cover")
  
  if (input_1 == 5 & input_2 == 1) return("Increased Tree Cover")
  if (input_1 == 5 & input_2 == 3) return("Increased Tree Cover")
  if (input_1 == 5 & input_2 == 6) return("Decreased Tree Cover")
  stop("New combination for tree cover change detected: ", input_1, " -> ", input_2)
}

# ______________________________________________________________________________
classify_land_use_intensity_change <- function(input_1, input_2){
  # Potential changes are:
  # df_tmp |> mutate(luc = paste0(land_use, "_", autut_2)) |> pull(luc) |> unique()
  
  # Easy Cases
  if (is.na(input_1) | is.na(input_2)) return(NA)
  if (input_1 == input_2) return("No Change")
  
  # Define order of intensity
  order_var <- c(
    "Wood Production",
    "Agriculture",
    "Infrastructure",
    "Military",
    "Hunting",
    "Leisure",
    "No Use",
    "Protected"
  )
  
  # Define function for comparing order
  compare_order <- function(var1, var2, order_var) {
    
    if (order_var[match(var1, order_var)] < order_var[match(var2, order_var)]) {
      return("increased")
      
    } else if (order_var[match(var1, order_var)] > order_var[match(var2, order_var)]) {
      return("decreased")
      
    } else {
      return("no change")
    }
  }
  
  # Apply function
  compare_order(input_1, input_2, order_var)
}


# Human impact --------------------------------------------------------------------------------

# New Variables:
# - human_activity: [NA, none, low, medium, high]
# - human_activity_var: indicator variable for activity assessment
# 
# Variable Hierarchy:
#   - PRELEV5: Main cutting indicator
#   - DEF5: Clearing indicator
#   - GEST: Management indicator
#   - ELAG: Pruning indicator
#   - NLISI5: Infrastructure establishment indicator
#   - INSTP5: Plantation work indicator
#   - ANDAIN: Windrow indicator

classify_human_activity <- function(
    prelev5,
    def5,
    gest,
    elag,
    nlisi5,
    instp5,
    andain){
  
  out <- tibble(
    human_activity     = NA,
    human_activity_var = NA
  )
  
  ### PRELEV5 ----
  if (!(prelev5 %in% c(NA))) {
    
    out$human_activity_var <- "PRELEV5"  
    
    if (prelev5 %in% c(0)) out$human_activity <- "none"
    if (prelev5 %in% c(1)) out$human_activity <- "medium"
    if (prelev5 %in% c(2)) out$human_activity <- "high"
    
    return(list(out))
  }
  
  ### DEF5 ----
  if (!(def5 %in% c(NA))) {
    
    out$human_activity_var <- "DEF5"  
    
    if (def5 %in% c(0)) out$human_activity    <- "none"
    if (def5 %in% c(1)) out$human_activity    <- "low"
    if (def5 %in% c(2, 3)) out$human_activity <- "medium"
    if (def5 %in% c(4, 5)) out$human_activity <- "high"
    
    return(list(out))
  }
  
  ### GEST ----
  if (!(gest %in% c(NA))) {
    
    out$human_activity_var <- "GEST"  
    
    if (gest %in% c(0)) out$human_activity <- "none"
    if (gest %in% c(1)) out$human_activity <- "low"
    if (gest %in% c(2)) out$human_activity <- "high"
    
    return(list(out))
  }
  
  ### ELAG ----
  if (!(elag %in% c(NA))) {
    
    out$human_activity_var <- "ELAG"  
    
    if (elag %in% c(0)) out$human_activity <- "none"
    if (elag %in% c(1)) out$human_activity <- "medium"
    
    return(list(out))
  }
  
  ### NLISI5 ----
  if (!(nlisi5 %in% c(NA))) {
    
    out$human_activity_var <- "NLISI5"  
    
    if (nlisi5 %in% c(0)) out$human_activity <- "none"
    if (nlisi5 %in% c(1)) out$human_activity <- "low"
    if (nlisi5 %in% c(2)) out$human_activity <- "medium"
    if (nlisi5 %in% c(3)) out$human_activity <- "high"
    
    return(list(out))
  }
  
  
  ### INSTP5 ----
  if (!(instp5 %in% c(NA))) {
    
    out$human_activity_var <- "INSTP5"  
    
    if (instp5 %in% c(0)) out$human_activity <- "none"
    if (instp5 %in% c("P", "S")) out$human_activity <- "medium"
    
    return(list(out))
  }
  
  ### ANDAIN ----
  if (!(andain %in% c(NA))) {
    
    out$human_activity_var <- "ANDAIN"  
    
    if (andain %in% c(0)) out$human_activity <- "none"
    if (andain %in% c(1)) out$human_activity <- "medium"
    
    return(list(out))
  }
  
  return(list(out))
}


# Nature impact -------------------------------------------------------------------------------

# TODO


# Dominant factor per site --------------------------------------------------------------------
# ______________________________________________________________________________
# ASSESS DOMINANT TREE SPECIES AND CLASS
get_dominant_factor_per_plot <- function(
    df_in,
    group_var,
    based_on_ntrees_or_totalba,
    mix_threshold
) {
  
  # Check input
  pot_group_vars <- c("genus_lat", "tree_class")
  if (!(group_var %in% pot_group_vars)) {
    stop(paste0(
      "\n> Invalid choice for `group_var`. Pick one of: ",
      paste0(pot_group_vars, collapse = ", ")
    ))
  }
  # 
  # needed_vars <- c("idp", "campagne_1", "tree_id", group_var, "tree_state_change")
  # if (based_on_ntrees_or_totalba == "totalba") needed_vars <- c(needed_vars, "ba_1")
  # 
  # if (!(all(needed_vars %in% names(df_in)))) {
  #   stop("> Input variable is missing!")
  # }
  # 
  # # Nest dataframe by site
  # df_tmp <- df_in |> group_by(idp, campagne_1) |> nest()
  
  # Assesement based on basal area
  if (based_on_ntrees_or_totalba == "totalba") {
    
    # Get total at first visit
    total <- sum(df_in$ba_1, na.rm = TRUE)
    
    if (is.na(total)) {
      message(paste0(
        "> Basal area has only missing data, is data filtered properly?",
        "\n  Returning NA"
      ))
      return(NA)
    }
    
    ttt <- 
      df_in |> 
      group_by_at(group_var) |> 
      summarise(sum = sum(ba_1)) |> 
      mutate(perc_ba = sum / total) |> 
      arrange(desc(sum)) |> 
      filter(perc_ba > mix_threshold)
    
    if (nrow(ttt) == 0) {
      dominant <- "none"
    } else {
      dominant <- ttt[[group_var]][1]
    }
    
    # Assessment based on number of trees
  } else if (based_on_ntrees_or_totalba == "ntrees") {
    
    # Get total at first visit
    total <- nrow(df_in)
    
    if (total == 0) {
      message(paste0(
        "> Basal area has only missing data, is data filtered properly?",
        "\n  Returning NA"
      ))
      return(NA)
    }
    
    ttt <- 
      df_in |> 
      group_by_at(group_var) |> 
      summarise(sum = n()) |> 
      mutate(perc_ba = sum / total) |> 
      arrange(desc(sum)) |> 
      filter(perc_ba > mix_threshold)
    
    if (ttt["broadleaf"] > mix_threshold) {
      dominant <- "broadleaf"
    } else if (ttt["broadleaf"] < (1 - mix_threshold)) {
      dominant <- "pinus"
    } else {
      dominant <- "mixed"
    }
  } else {
    # Catch error
    stop("> Invalid calculation reference.")
  }
  
  # Return
  return(as.character(dominant))
  
}

