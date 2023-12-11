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
  pot_group_vars <- c("genus_lat", "tree_class", "species_lat", "espar_red")
  # if (!(group_var %in% pot_group_vars)) {
  #   stop(paste0(
  #     "\n> Invalid choice for `group_var`. Pick one of: ",
  #     paste0(pot_group_vars, collapse = ", ")
  #   ))
  # }
  
  # Assesement based on basal area
  if (group_var %in% pot_group_vars) {
    
    if (based_on_ntrees_or_totalba == "totalba") {
      
      # Initialise output dataframe
      df <- tibble(
        !!paste0("dom_nr1_", group_var, "_", "fct")    := "none",
        !!paste0("dom_nr1_", group_var, "_", "ba_abs") := 0,
        !!paste0("dom_nr1_", group_var, "_", "ba_prc") := 0,
        
        !!paste0("dom_nr2_", group_var, "_", "fct")    := "none",
        !!paste0("dom_nr2_", group_var, "_", "ba_abs") := 0,
        !!paste0("dom_nr2_", group_var, "_", "ba_prc") := 0,
        
        !!paste0("dom_nr3_", group_var, "_", "fct")    := "none",
        !!paste0("dom_nr3_", group_var, "_", "ba_abs") := 0,
        !!paste0("dom_nr3_", group_var, "_", "ba_prc") := 0,
        
        !!paste0("dom_rest_", group_var, "_", "fct")    := "none",
        !!paste0("dom_rest_", group_var, "_", "ba_abs") := 0,
        !!paste0("dom_rest_", group_var, "_", "ba_prc") := 0
      )
      
      # Get total at first visit
      total <- sum(df_in$ba_1, na.rm = TRUE)
      
      if (is.na(total)) {
        message(paste0(
          "> Basal area has only missing data, is data filtered properly?",
          "\n  Returning NA dataframe"
        ))
        return(df)
      }
      
      # Get dominance order 
      ttt <- 
        df_in |> 
        group_by_at(group_var) |> 
        summarise(sum = sum(ba_1, na.rm = TRUE)) |> 
        mutate(perc_ba = sum / total) |> 
        arrange(desc(sum))
      
      # Populate output dataframe
      # Define cut-off of how many species should be named explicit
      cut_off <- 3 # Three explicit species, rest added under "remaining"
      
      for (i in 1:min(cut_off, nrow(ttt))) {
        df[paste0("dom_nr", i, "_", group_var, "_", "fct")]    <- ttt[i, 1]
        df[paste0("dom_nr", i, "_", group_var, "_", "ba_abs")] <- ttt[i, 2]
        df[paste0("dom_nr", i, "_", group_var, "_", "ba_prc")] <- ttt[i, 3]
      }
      
      if (nrow(ttt) > cut_off) {
        df[paste0("dom_rest_", group_var, "_", "fct")]    <- "remaining"
        df[paste0("dom_rest_", group_var, "_", "ba_abs")] <- sum(ttt[i, 2], na.rm = TRUE)
        df[paste0("dom_rest_", group_var, "_", "ba_prc")] <- sum(ttt[i, 3], na.rm = TRUE)
      }
    }
    
  # ________________________________________________________________________________________________
  } else if (group_var %in% c("tca", "tcl")) {
    
    # Initialise output dataframe
    df <- tibble(
      !!paste0("dom_nr1_", group_var, "_", "fct") := "none",
      !!paste0("dom_nr1_", group_var, "_", "prc") := 0,
      
      !!paste0("dom_nr2_", group_var, "_", "fct") := "none",
      !!paste0("dom_nr2_", group_var, "_", "prc") := 0,
      
      !!paste0("dom_nr3_", group_var, "_", "fct") := "none",
      !!paste0("dom_nr3_", group_var, "_", "prc") := 0,
      
      !!paste0("dom_rest_", group_var, "_", "fct") := "none",
      !!paste0("dom_rest_", group_var, "_", "prc") := 0,
    )
    
    # Get temporary df
    ttt <-
      df_in |> 
      group_by_at("espar_red") |> 
      summarise(sum = sum(!!sym(group_var), na.rm = TRUE)) |> 
      arrange(desc(sum))
    
    # Populate output dataframe
    # Define cut-off of how many species should be named explicit
    cut_off <- 3 # Three explicit species, rest added under "remaining"
    
    for (i in 1:min(cut_off, nrow(ttt))) {
      df[paste0("dom_nr", i, "_", group_var, "_", "fct")] <- ttt[i, "espar_red"]
      df[paste0("dom_nr", i, "_", group_var, "_", "prc")] <- ttt[i, "sum"]
    }
    
    if (nrow(ttt) > cut_off) {
      df[paste0("dom_nr", i, "_", group_var, "_", "fct")] <- "remaining"
      df[paste0("dom_nr", i, "_", group_var, "_", "prc")] <- sum(ttt[i, "sum"], na.rm = TRUE)
    }
  }
  
  # Assessment based on number of trees
  # TODO: Outcommented on 2023-11-27 to focus only on BA dominance
  # else if (based_on_ntrees_or_totalba == "ntrees") {
  #   
  #   # Get total at first visit
  #   total <- nrow(df_in)
  #   
  #   if (total == 0) {
  #     message(paste0(
  #       "> Basal area has only missing data, is data filtered properly?",
  #       "\n  Returning NA"
  #     ))
  #     return(df)
  #   }
  #   
  #   # Get dominance order
  #   ttt <- 
  #     df_in |> 
  #     group_by_at(group_var) |> 
  #     summarise(sum = n()) |> 
  #     mutate(perc_ba = sum / total) |> 
  #     arrange(desc(sum)) |> 
  #     filter(perc_ba > mix_threshold)
  #   
  #   if (ttt["broadleaf"] > mix_threshold) {
  #     dominant <- "broadleaf"
  #   } else if (ttt["broadleaf"] < (1 - mix_threshold)) {
  #     dominant <- "pinus"
  #   } else {
  #     dominant <- "mixed"
  #   }
  # } else {
  #   # Catch error
  #   stop("> Invalid calculation reference.")
  # }
  
  # Return
  return(df)
}

get_site_metrics_of_top_3_species <- function(
    df_in = NULL, 
    vars_in = c("ba_1", "age13", "ir5", "v", "htot"),
    metrics_in = c("mean", "sd", "range")
) {
  
  # Checks
  if (is.null(df_in)) stop("df_in needed!")
  
  supported_metrics <- c("mean", "sd", "range")
  for (met in metrics_in) {
    if (!(met %in% supported_metrics)) {
      stop(paste0("Requested metric is not supported yet: ", met))
    }
  }
  
  for (var in vars_in) {
    if (!(var %in% names(df_in))) {
      stop(paste0("Requested variable is not in df_in: ", var))
    }
  }
  
  # Change factor to character because of "none" level if there is no nth species
  df_in$espar_red <- as.character(df_in$espar_red)
  
  # Create empty df
  df <- tibble(.rows = 1)
  
  df["n_species_nfi"] <- length(unique(df_in$espar_red))
  
  for (i in 1:3) {
    df[paste0("top", i, "_species_nfi")] <- "none"
    for (var in vars_in) {
      for (metric in metrics_in) {
        df[paste0("top", i, "_species_", var, "_", metric)] <- NA
      }
    }
  }
  
  # Get total basal area:
  total_ba_1 <- df_in$ba_1 |> sum(na.rm = TRUE)
  
  # Get dominant species
  dom_spec <- 
    df_in |>
    group_by(espar_red) |> 
    summarise(sum = sum(ba_1, na.rm = TRUE)) |> 
    ungroup() |> 
    arrange(desc(sum)) |> 
    slice(1:3) |> 
    pull(espar_red)
  
  # Populate dataframe
  for (i in 1:length(dom_spec)) {
    df[paste0("top", i, "_species")] <- dom_spec[i]
    for (var in vars_in) {
      for (metric in metrics_in) {
        
        my_vec <- df_in |> filter(espar_red == dom_spec[i]) |> pull(!!var)
        
        if (metric == "mean") value <- mean(my_vec, na.rm = TRUE)
        if (metric == "sd")   value <- sd(my_vec, na.rm = TRUE)
        # cat("\n", "top", i, "_species_", var, "_", metric, ": \t", value, sep = "") # Verbose
        
        # Normalise value by the ba_1 of that species so that the value 
        #  is inter-comparable across the dominant species at different sites
        #  Alternative: Divide by species ba
        #  i_ba  <- df_in |> filter(espar_red == dom_spec[i]) |> pull(ba_1) |> sum(na.rm=TRUE)
        if (metric %in% c("mean", "sd")) value <- value / total_ba_1
        
        if (metric == "range") value <- max(my_vec, na.rm = TRUE) - min(my_vec, na.rm = TRUE)
        
        df[paste0("top", i, "_species_", var, "_", metric)][1] <- value
        
        # To avoid NaN and NA mixing up
        if (is.nan(value)) value <- NA
      }
    }
  }
  
  return(df)
}
