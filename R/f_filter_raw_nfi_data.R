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
  
  # Add percentage of survived / lost / cut and dominant species / tree type
  message(paste0("> filter_raw_nfi_data():",
                 "\n  ... assessing dominant species and class takes ~10 minutes\n  ... until: ",
                 format((Sys.time() + minutes(10)), format = "%H:%M:%S")
                 ))
  
 
  tic() 
  df_tmp <- 
    df_tmp |> 
    dplyr::select(idp, ba_1, tree_state_change, genus_lat, tree_class) |> 
    filter(tree_state_change %in% c("alive_alive", "alive_dead", "alive_cut")) |> 
    nest(data = -idp) |> 
    # slice(1:20) |> 
    mutate(
      
      # Percentages:
      # Note: Reduced to cutting only because the others are covered in growth and mortality calculations
      tot       = map_dbl(data, ~pull(., ba_1) |> sum(na.rm = T)),
      # perc_surv = map_dbl(data, ~filter(., tree_state_change == "alive_alive") |> pull(ba_1) |> sum(na.rm = T)) / tot,
      # perc_died = map_dbl(data, ~filter(., tree_state_change == "alive_dead")  |> pull(ba_1) |> sum(na.rm = T)) / tot,
      perc_cut  = map_dbl(data, ~filter(., tree_state_change == "alive_cut")   |> pull(ba_1) |> sum(na.rm = T)) / tot,
      
      # Dominance:
      dominant_species = 
         map_chr(
           data,
           ~get_dominant_factor_per_plot(
             .,
             group_var = "genus_lat", 
             based_on_ntrees_or_totalba = "totalba",
             mix_threshold = 0.6
           ),
           .progress = TRUE
         ),
       dominant_tree_class = 
         map_chr(
           data,
           ~get_dominant_factor_per_plot(
             .,
             group_var = "tree_class", 
             based_on_ntrees_or_totalba = "totalba",
             mix_threshold = 0.6
           ),
           .progress = TRUE
         )) |> 
    dplyr::select(-data, -tot) |> 
    right_join(df_tmp, by = join_by(idp))
  toc()
  
  # Remove faulty ∆ calculations
  df_tmp <- df_tmp |> filter(dbh_change_perc_yr >= 0 | is.na(dbh_change_perc_yr)) 
  
  # Remove outliers
  df_tmp <- df_tmp |> filter(dbh_change_perc_yr < quantile(dbh_change_perc_yr, 0.99, na.rm = TRUE)) 
  
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
  load_or_save_latest_file(nfi_dataset_for_analysis, "save")
  message("\n  ... Done! Saved nfi_dataset_for_analysis.")
  
  return(nfi_dataset_for_analysis)
}
