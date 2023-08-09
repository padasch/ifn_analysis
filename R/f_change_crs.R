f_change_crs <-function(data, from_CRS, to_CRS) {
  
  # Check if input is correct
  if (!all(c("lat", "lon", "id") %in% names(data))){
    stop("Incorrect data input. Required are lat, lon, id.")
  }
  
  if (!str_detect(from_CRS, '^[0-9]{4}$') |
      !str_detect(to_CRS, '^[0-9]{4}$')){
    stop("Incorrect CRS input. Requires four digit EPSG code.")
  }
  
  from_CRS <- paste0("+init=epsg:", from_CRS)
  to_CRS <- paste0("+init=epsg:", to_CRS)
  
  # Load required package
  require(sp)
  
  # Turn string into CRS
  from_CRS = CRS(from_CRS)
  to_CRS   = CRS(to_CRS)
  
  new <- 
    as.data.frame(
      spTransform(
        SpatialPointsDataFrame(
          coords = data.frame(
            lon = data$lon,
            lat = data$lat),
          
          data = data.frame(
            id = data$id,
            lon_old = data$lon,
            lat_old = data$lat),
          proj4string = from_CRS), 
        to_CRS
      )
    )
  
  new <- 
    new |> 
    dplyr::select(coords.x1, coords.x2, id) |> 
    dplyr::rename(lon = coords.x1,
                  lat = coords.x2)
  
  return(new)
}
