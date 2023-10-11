calculate_growth_mortality_per_hexagon <- function(
    df_species = NULL,
    n_polygons_per_degree = NULL,
    min_ntrees_per_polygon = NULL
  ) {
  
  # Input Checks ----
  # ______________________________________________________________________________
  # n_polygons_per_degree:
  #   - Numeric, defines grid size, number specifies
  #   - How many polygons per degree should be created.
  # min_ntrees_per_polygon:
  #   - Minimum number of trees that must be contained in a polygon to calculate
  #     growth or mortality
  # df_species
  #   - tibble of forestry data at the tree level, having information on
  #     tree basal area, coordinates, etc.
  
  if (!is.numeric(n_polygons_per_degree)) {stop("Input for n_polygons_per_degree is wrong!")}
  if (!is.numeric(min_ntrees_per_polygon)) {stop("Input for min_ntrees_per_polygon is wrong!")}
  if (!is_tibble(df_species)) {stop("Input for df_species is wrong!")}
  
  # Get shapefile of france
  sf_france <- get_shapefile_france() |> st_transform(crs = 2192)
  
  # DEBUG: Plotting check
  # ggplot(sf_france, aes(fill = NAME_1)) + geom_sf()
  
  # Make hex-grid over France ----
  # ______________________________________________________________________________
  grd <- 
    st_make_grid(
      sf_france,
      n = c(n_polygons_per_degree, n_polygons_per_degree),
      what = "polygons",
      square = FALSE
    )  |> 
    st_sf()  |>  # convert back to sf object
    mutate(polyg_id = row_number()) # add a unique id to each cell
  
  # Extract centroids
  cent <- grd |> st_centroid()
  
  # DEBUG: Plotting Check Take a look at the results
  # ggplot() +
  #   geom_sf(grd, mapping = aes(geometry = geometry)) +
  #   geom_sf(
  #     cent,
  #     mapping = aes(geometry = geometry),
  #     pch = 21,
  #     size = 0.5
  #   )
  
  # Intersect centroids with basemap
  cent_clean <- cent |> st_intersection(sf_france)
  
  # Make a centroid without geom
  # (convert from sf object to tibble)
  cent_no_geom <- cent_clean |> st_drop_geometry()
  
  # Join with grid thanks to id column
  grd_clean <- grd |> left_join(cent_no_geom, by = join_by(polyg_id))
  
  # DEBUG: Check result
  # drop_na() is one way to suppress the cells outside the country
  # ggplot() +
  #   geom_sf(
  #     grd_clean |>
  #       drop_na(NAME_1) |>
  #       mutate(NAME_1 = ifelse(NAME_1 == "Normandie", NA, NAME_1)),
  #     mapping = aes(geometry = geometry, fill = NAME_1)
  #   ) +
  #   geom_sf(cent_clean,mapping=aes(geometry=geometry),fill=NA,pch=21,size=0.5)+
  #   labs(fill = "Departement") +
  #   theme_void()
  
  # Aggregate forestry data ----
  # ______________________________________________________________________________
  # Joining forest data with hexmap data to match geometry id
  df_joint_sf <- 
    st_as_sf(
      df_species,
      coords = c("lon_fr", "lat_fr"),
      crs = st_crs(grd_clean)) |>
    st_join(grd_clean)
  
  # Filter:
  # - Keep only trees that were alive at 1st census and not cut at 2nd census
  # - Filter for sites that have at least `min_ntrees_per_polygon` trees of the species
  
  df_nested_by_polygon <-
    df_joint_sf |>
    mutate(
      census_interval = paste0(campagne_1, "-", campagne_1 + 5),
      census_interval = as.factor(census_interval)
    ) |>
    filter(
      tree_state_1 == "alive", 
      tree_state_2 != "cut"
    ) |>
    group_by(polyg_id, census_interval) |>
    filter(n() >= min_ntrees_per_polygon) |>
    nest()
  
  # Calculate growth and mortality per polygon
  message("Aggregating forest data to polygon level, this may take some time...")
  tic()
  
  df_change_per_polygon <-
    df_nested_by_polygon |>
    mutate(
      n_ini = map_dbl(data,
                      ~ filter(., revisit_state == "revisited") |>
                        nrow()),
      
      n_fin = map_dbl(data, ~ filter(., tree_state_2 == "alive") |>
                        nrow()),
      
      n_sur = map_dbl(
        data,
        ~ filter(., revisit_state == "revisited", tree_state_2  == "alive") |>
          nrow()
      ),
      
      ba_at_v1_of_alive_trees = map_dbl(
        data,
        ~ filter(., revisit_state == "revisited") |>
          pull(ba_1) |> sum()
      ),
      
      ba_at_v2_of_alive_trees = map_dbl(data,
                                        ~ filter(., tree_state_2 == "alive") |>
                                          pull(ba_2) |> sum()),
      
      ba_at_v1_of_surivors = map_dbl(
        data,
        ~ filter(., revisit_state == "revisited", tree_state_2  == "alive") |>
          pull(ba_1) |> sum()
      ),
      
      ba_at_v2_of_survivors = map_dbl(
        data,
        ~ filter(., revisit_state == "revisited", tree_state_2  == "alive") |>
          pull(ba_2) |> sum()
      ),
      
      ba_at_v2_of_dead = map_dbl(
        data,
        ~ filter(., revisit_state == "revisited", tree_state_2  == "dead") |>
          pull(ba_2) |> sum()
      ),
      
      n_mor_yr      = log(n_ini / n_sur) / 5 * 100,
      n_mor_yr_esq  = (1 - (n_sur / n_ini) ^ (1 / 5)) * 100,
      n_rec_yr      = log(n_fin / n_sur) / 5 * 100,
      
      ba_loss_yr    = log(ba_at_v1_of_alive_trees / ba_at_v1_of_surivors) / 5 * 100,
      ba_gain_yr    = log(ba_at_v2_of_alive_trees / ba_at_v1_of_surivors) / 5 * 100,
      ba_ingr_yr    = log(ba_at_v2_of_survivors / ba_at_v1_of_surivors) / 5 * 100,
      
      ba_growth_abs = (ba_at_v2_of_survivors - ba_at_v1_of_surivors) / 5, # m2 tree / hectare / yr
      ba_loss_abs   = ba_at_v2_of_dead / 5, # m2 tree / hectare / yr
      
      ba_growth_rate = ba_growth_abs / ba_at_v1_of_alive_trees * 100, # % rate
      ba_loss_rate   = ba_loss_abs   / ba_at_v1_of_alive_trees * 100 # % rate
    ) |>
    ungroup()
  
  toc()
  
  # Quality Control ----
  # ______________________________________________________________________________
  # - Remove negative values for growth and loss, because non-sensical
  df_change_per_polygon <- 
    df_change_per_polygon |> 
    filter(
      ba_growth_abs > 0,
      ba_loss_abs   > 0,
    )
  
  # Add geometry ----
  # ______________________________________________________________________________
  df_hexmap <-
    left_join(
      df_change_per_polygon |> select(-data),
      grd_clean,
      by = join_by(polyg_id)
      )
  
  # Match data to regions
  all_deps      <- grd_clean |> pull(polyg_id) |> unique()
  dummy         <- c(2015:2021)
  all_intervals <- paste0(dummy - 5, "-", dummy)
  
  df_grid   <-
    expand.grid(all_deps, all_intervals) |> 
    rename(polyg_id = Var1, census_interval = Var2) |> 
    as_tibble()
  
  df_grid <- 
    left_join(
      df_grid, 
      df_change_per_polygon |> select(-data),
      by = join_by(polyg_id, census_interval))
  
  # Attach data
  df_plot <- 
    right_join(
      grd_clean, 
      df_grid,
      by = "polyg_id")
  
  return(df_plot)
}
