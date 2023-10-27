gee_get_france_boundaries <- function() {
  
  gee_crs   <- 3395 # gee uses mercator 3395 as default
  sf_france <- get_shapefile_france(which_region = "fra") |> st_transform(gee_crs)
  box_fr    <- st_bbox(sf_france)
  
  # Add some padding around study area
  box_fr["xmin"] <- box_fr["xmin"] - box_fr["xmin"] * 0.005
  box_fr["ymin"] <- box_fr["ymin"] - box_fr["ymin"] * 0.005
  box_fr["xmax"] <- box_fr["xmax"] + box_fr["xmax"] * 0.01
  box_fr["ymax"] <- box_fr["ymax"] + box_fr["ymax"] * 0.005
  
  # Set region of interest
  r_roi <- c(
    box_fr$xmin,
    box_fr$ymin,
    box_fr$xmax,
    box_fr$ymin,
    box_fr$xmax,
    box_fr$ymax,
    box_fr$xmin,
    box_fr$ymax,
    box_fr$xmin,
    box_fr$ymin
  )
  
  roi <-
    matrix(r_roi, ncol = 2, byrow = TRUE) %>%
    list() %>%
    st_polygon() %>%
    st_sfc() %>%
    st_set_crs(gee_crs) %>%
    sf_as_ee()
  
  roi
}
