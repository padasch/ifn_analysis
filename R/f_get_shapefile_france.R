get_shapefile_france <- function(outer_border_only = FALSE) {
  
  if (outer_border_only) {
    sf_france <- 
      ne_countries(
        geounit = "france", 
        type = "map_units", 
        scale = 10
      ) |> 
      st_as_sf() |>
      select(geounit, geometry)
    
  } else {
    sf_france <- raster::getData(country = 'France', level = 2, path = "data/tmp/")
    sf_france <- sf::st_as_sf(sf_france)
  }
  
  
  return(sf_france)
}
