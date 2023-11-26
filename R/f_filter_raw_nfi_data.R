filter_raw_nfi_data <- function(
    only_suitable_sites  = TRUE,
    only_suitable_trees  = TRUE,
    remove_shadow_growth = TRUE,
    remove_missing_coord = TRUE,
    target_metric        = "ba"
){
  
  # Check input
  if (!(target_metric %in% c("ba"))) {
    stop("Chosen target_metric not supported yet.")
  }
  
  # Load latest raw dataset
  load_or_save_latest_file(nfi_dataset_raw, "load")
  df_tmp <- nfi_dataset_raw
  
  # Remove faulty ∆ calculations
  df_tmp <- df_tmp |> filter(dbh_change_perc_yr >= -0.05 | is.na(dbh_change_perc_yr)) 
  
  # ______________________________________________________________________________
  # Keep only suitable sites 
  # *Decisions:*
  #  - PRELEV5: Keep only sites that were not cleared or where NA
  #  - PEUPNR: Keep only sites that were censusable or where NA
  #  - CSA: No need to filter, plots are from wooded areas (that may have changed into sthg else).
  #  - LAT/LON: Only keep sites for which we have coordinates
  
  df_tmp <- 
    df_tmp |> 
    filter(
      prelev5  %in% c(0, 1, NA),
      peupnr_1 %in% c(0, NA),
      peupnr_2 %in% c(0, NA),
      csa_1    %in% c(1, 2, 3, 5, NA),
      !is.na(lat),
      !is.na(lon),
    )
  
  # Keep only sites measured first in 2010 for simplicity reasons
  df_tmp <-
    df_tmp |>
    mutate(campagne_1 = as.numeric(as.character(campagne_1))) |> 
    filter(campagne_1 > 2009) |> 
    mutate(campagne_1 = as.factor(campagne_1))
  
  # ______________________________________________________________________________
  # Keep only suitable trees
  # *Decisions*
  #   - Remove shadow_growth for now
  #   - Keep only trees after 2009 (most are out anyways due to other filters)
  #   - Keep only survivors, deaths, recruits
  #   - Keep only those who fell within sampling target
  
  if (remove_shadow_growth) {df_tmp <- df_tmp |> filter(shadow_growth == "no")}
  
  # Keep only survivors, deaths, recruits
  df_tmp <- 
    df_tmp |>
    filter(
      tree_state_change %in% c("alive_alive", "alive_dead", "new_alive"),
      cible %in% c(1) # NA is omitted
    ) 
  
  nfi_dataset_for_analysis <- df_tmp 
  return(nfi_dataset_for_analysis)
}
