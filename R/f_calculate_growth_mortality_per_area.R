calculate_growth_mortality_per_area <- function(
    df_species = NULL,
    region_type = NULL,
    n_poly = NULL,
    min_ntrees_per_region = NULL
  ) {
  
  # ______________________________________________________________________________
  # Input Checks ----
  # region_type:
  #   - Specify the regions over which data should be aggregated.
  #   - Options: polygons, departments, regions, sylvoregions, grecos
  # n_poly:
  #   - Numeric, defines grid size, number specifies
  #   - How many polygons per degree should be created.
  # min_ntrees_per_region:
  #   - Minimum number of trees that must be contained in a polygon to calculate
  #     growth or mortality
  # df_species
  #   - tibble of forestry data at the tree level, having information on
  #     tree basal area, coordinates, etc.
  
  if (!is.numeric(min_ntrees_per_region)) {
    message("> No input for min_ntrees_per_region! Taking default: 10")
    min_ntrees_per_region <- 10
    }
  if (!is_tibble(df_species)) {
    stop("Input for df_species is wrong!")
    }
  if (is.null(region_type)) {
    message("> No region_type selected (dep, reg, ser, gre, hex), using standard: hexagons")
    region_type <- "hex"
    }  
  if (region_type == "hex" & is.null(n_poly)) {
    message("> No hexagon size selected, using standard: 15")
    n_poly <- 15
    }  
  
  # ______________________________________________________________________________
  # Get shapefile of interest ----
  sf_france <- get_shapefile_france(which_region = region_type, n_poly = n_poly)
  
  # First variable in sf_france is always the grouping variable of interest, define it as such:
  names(sf_france)[1] <- "grouping_region"
  
  # Aggregate forestry data ----
  # ______________________________________________________________________________
  # Joining forest data with hexmap data to match geometry id
  df_joint_sf <- 
    st_as_sf(
      df_species,
      coords = c("lon_fr", "lat_fr"),
      crs = st_crs(sf_france)) |>
    st_join(sf_france)
  
  # Filter for sites that have at least `min_ntrees_per_region` trees of the species
  df_region <-
    df_joint_sf |>
    group_by(grouping_region, census_interval) |>
    filter(n() >= min_ntrees_per_region) |>
    nest()
  
  # ______________________________________________________________________________
  # Calculate growth and mortality per polygon
  message("> Aggregating trees to regional level, this may take some time...")
  tic()
  df_region_change <- df_region |> calculate_growth_mortality() |> ungroup()
  toc()
  
  # Quality Control ----
  # ______________________________________________________________________________
  # - Remove negative values for growth and loss, because non-sensical
  df_region_change <- 
    df_region_change |> 
    filter(
      ba_growth_abs > 0,
      ba_loss_abs   > 0,
    )
  
  # Add geometry ----
  # ______________________________________________________________________________
  df_region_change_map <-
    left_join(
      df_region_change |> select(-data),
      sf_france,
      by = join_by("grouping_region")
      )
  
  # Match data to regions
  all_regions   <- sf_france |> pull(grouping_region) |> unique()
  dummy         <- c(2015:2021)
  all_intervals <- paste0(dummy - 5, "-", dummy)
  
  df_grid   <-
    expand.grid(all_regions, all_intervals) |> 
    rename(grouping_region = Var1, census_interval = Var2) |> 
    as_tibble()
  
  df_plot <- 
    left_join(
      left_join(df_grid, sf_france) |> select(grouping_region, census_interval, geometry), 
      df_region_change_map |> select(-geometry),
      by = join_by("grouping_region", "census_interval")) |> 
    st_as_sf()
  
  return(df_plot)
}
