source(here::here("R/_setup.R"))
load_or_save_latest_file(nfi_dataset_for_analysis, "load")

# ______________________________________________________________________________
my_min_trees_per_site <- 3
my_min_sites_per_year <- 5

tt_height_gre <- 
  get_temporal_trends_for_2_groups(
    df_in              = nfi_dataset_for_analysis,
    name_group_1       = "height_class",
    name_group_2       = "gre",
    n_groups_1         = 10,
    n_groups_2         = 9,
    min_trees_per_site = my_min_trees_per_site,
    min_sites_per_year = my_min_sites_per_year
  )

tt_gre_height <- 
  get_temporal_trends_for_2_groups(
    df_in              = nfi_dataset_for_analysis,
    name_group_1       = "gre",
    name_group_2       = "height_class",
    n_groups_1         = 10,
    n_groups_2         = 9,
    min_trees_per_site = my_min_trees_per_site,
    min_sites_per_year = my_min_sites_per_year
  )

tt_save_and_move_to_shiny(tt_height_gre, my_height = 7, my_width = 12)
tt_save_and_move_to_shiny(tt_gre_height, my_height = 7, my_width = 12)
# ______________________________________________________________________________
beepr::beep(2)
