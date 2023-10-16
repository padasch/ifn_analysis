match_coordinates_to_french_region <- function(df_input){
  
  # Check input
  if (!all(c("lat", "lon") %in% names(df_input))) {
    stop("match_coordinates_to_french_region(): Input data is missing lat or lon.")
  }
  
  # Get the shapefile data for France
  sf_france <- get_shapefile_france("dep") |> st_transform("WGS84")
  
  # Wrangle input dataframe to temporary one
  df_tmp <-
    df_input |> 
    select(lat, lon) |> 
    drop_na() |> 
    distinct() |> 
    rename(Lat = lat,
           Lon = lon) |> 
    mutate(id = row_number())
  
  # Convert df_tmp to an sf object with the same CRS as sf_france
  locations_sf <- sf::st_as_sf(df_tmp, coords = c("Lon", "Lat"), crs = st_crs(sf_france))
  
  # Ensure that locations_sf and sf_france have the same CRS
  locations_sf <- sf::st_transform(locations_sf, crs = st_crs(sf_france))
  
  # Spatial join sf_france with locations_sf (reversed order)
  locations_with_regions <- sf::st_join(locations_sf, sf_france)
  
  # Extract locations where coordinates could not be matched to a region
  # Get index
  na_idx <- 
    locations_with_regions |> 
    filter(is.na(dep)) |> 
    pull(id)
  
  if (length(na_idx) != 0) {
    # Filter df_tmp for index
    df_naloc <- 
      df_tmp |> 
      filter(id %in% na_idx)  |> 
      sf::st_as_sf(
        coords = c("Lon", "Lat"), 
        crs = st_crs(sf_france))
    
    # Calculate distance to the closest polygon and take its information.
    # Has to be done for each location, so using a loop here.
    df_new <- tibble()
    
    for (i in 1:nrow(df_naloc)) {
      
      # Get information on closest polygon
      df_i   <- 
        sf_france[which.min(st_distance(sf_france, df_naloc[i,])),] |> 
        as_tibble() |> 
        select(dep)
      
      # Attach polygon information to location dataframe
      df_i <- cbind(df_naloc[i, ], df_i)
      
      # Save loop-internal dataframe to outter dataframe
      df_new <- rbind(df_new, df_i)
    }  
    
    # Attach dataframes together again  
    df_final <- 
      locations_with_regions |> 
      filter(!(id %in% na_idx)) |> 
      bind_rows(df_new)
    
  } else {
    df_final <- locations_with_regions
  }
    
  
  # Turn df_final from sf into tibble
  df_final <- 
    bind_cols(
      # Extract lat lon data
      df_final |>
        st_coordinates() |> 
        as_tibble() |> 
        rename(lat = Y, lon = X),
      
      # Replace geometry by lat lon data
      df_final |> 
        as_tibble() |> 
        select(-geometry)
    )
  
  return(df_final)
}
