get_shapefile_france <- function() {
  
  sf_france <- raster::getData(country = 'France', 
                               level = 2, 
                               path = "data/tmp/")
  sf_france <- sf::st_as_sf(sf_france)
  
  return(sf_france)
}
