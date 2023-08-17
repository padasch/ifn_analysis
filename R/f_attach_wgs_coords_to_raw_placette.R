f_attach_wgs_coords_to_raw_placette <- function(df) {
  #' @param df must be l_raw_data$placette
   
  # Extract sampling coordinates in Lambert 93 CRS EPSG:2154
  coords_l93 <- 
    df |> 
    dplyr::select(lon, lat) |> 
    distinct() |> 
    rowid_to_column(var = "id")
  
  # Create simple feature
  sf_l93 <- sf::st_as_sf(coords_l93, coords = c("lon", "lat"), crs = 2154)
  
  # Transform to WGS84
  coords_wgs84     <- sf::st_transform(sf_l93, crs = 4326)
  coords_wgs84$lat_new <- sf::st_coordinates(coords_wgs84)[, "Y"]
  coords_wgs84$lon_new <- sf::st_coordinates(coords_wgs84)[, "X"]
  coords_wgs84 <- coords_wgs84 |> as_tibble() |> select(id, lat_new, lon_new)
  
  # Get coordinates dictionary
  coord_dict <- left_join(coords_l93, coords_wgs84, by = "id") |> select(-id)
  
  # Update location dataframe
  df <- 
    df |> 
    left_join(coord_dict, by = c("lon", "lat")) |> 
    dplyr::rename(lon_fr = lon, lat_fr = lat) |> 
    dplyr::rename(lon = lon_new, lat = lat_new) |> 
    dplyr::mutate(lon = round(lon, 6),
                  lat = round(lat, 6))
  
  return(df)
}


