# get_human_impact_information <- function(df_input) {
#
#
#
#
# }
#

# Input
if (TRUE) {
  message("⚠️ get_human_impact_information() is not loaded because it is not finished for now!")    
} else {
  
  warning("⚠️ This routine currently assesses only whether there are human influences detectable or not.")
  
  df_input <- df_comb |> slice_sample(n = 1)
  
  # Temporary df
  hum_vars <- get_vars("human")
  
  df_tmp <-
    df_input |>
    select(tree_id, contains(hum_vars))
  
  # V1 ----
  # High-level influence factor
  v1_l1_human <- NA
  
  # Medium-level influence factor
  v1_l2_clearcut <- NA
  v1_l2_partialcut <- NA
  v1_l2_managed <- NA
  
  # Low-level influence factor
  
  ## Cutting
  ### DC_1 ----
  if (!identical(df_tmp$dc_1, NA)) {
    if ((!identical(df_tmp$dc_1, 0))) {
      v1_l2_clearcut   <- 0
      v1_l2_partialcut <- 0
    } else {
      v1_l2_clearcut   <- 1
      v1_l2_partialcut <- 1
    }
  }
  
  ### Management
  mgmt_indicators <- 
    c(
      df_tmp$andain,
      df_tmp$elag,
      df_tmp$gest,
      df_tmp$tplant,
      df_tmp$tetard
    )
  
  if (!all(is.na((mgmt_indicators)))) {
  }
  
  
  
  
  # V2 ----
  
  v2_cut_intensity  <- NA
  v2_mgmt_intensity <- NA
  
  ## Cutting ----
  # Note: Assignment of variables is hierarchical prelev5, def5, dc_2
  
  ### prelev5 ----
  if (!identical(df_tmp$prelev5, NA)) {
    if ((!identical(df_tmp$prelev5, 0))) {
      v2_cut_intensity <- "none"
    } else if ((!identical(df_tmp$prelev5, 1))) {
      v2_cut_intensity <- "partial"
    } else if ((!identical(df_tmp$prelev5, 2))) {
      v2_cut_intensity <- "clearcut"
    }
  }
  
  ### def5 ----
  if (!identical(v2_cut_intensity, NA) |
      !identical(df_tmp$def5, NA)) {
    if ((!identical(df_tmp$def5, 0))) {
      v2_cut_intensity <- "none"
    } else if (df_tmp$def5 %in% c(1, 2, 3)) {
      v2_cut_intensity <- "partial"
    } else if (df_tmp$def5 %in% c(4, 5)) {
      v2_cut_intensity <- "clearcut"
    }
  }
  
  ### DC_2 ----
  if (!identical(v2_cut_intensity, NA) |
      !identical(df_tmp$dc_2, NA)) {
    if ((!identical(df_tmp$dc_2, 0))) {
      v2_cut_intensity <- "none"
    } else if (df_tmp$dc_2 %in% c(4, 5, 7, 8)) {
      v2_cut_intensity <- "partial"
    } else if (df_tmp$dc_2 %in% c(1, 2, 3, 4, 6, 9)) {
      v2_cut_intensity <- "clearcut"
    }
  }
  
  ## Management ----
  if (!identical(df_tmp$instp5, NA) |
      !identical(df_tmp$nlisi5, NA)) {
    if (!identical(df_tmp$instp5, 0) |
        !identical(df_tmp$nlisi5, 0)) {
      v2_mgmt_intensity <- 0
    } else {
      v2_mgmt_intensity <- 1
    }
  }
  
  
  
  
  
}
