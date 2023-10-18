get_shapefile_france <- function(which_region = NA, n_poly = 15) {
  
  # ______________________________________________________________________________
  # Error
  possible_regions <- c("fra", "dep", "reg", "ser", "gre", "hex")
  
  if (which_region %in% c(NA)) {
    stop(
      "Invalid shapefile request.\n > Select one of these options: ", 
      paste0(possible_regions, sep = ", ")
      )
    }
  
  # ______________________________________________________________________________
  # Country
  if (which_region == "fra") {
    
    sf_france <- 
      ne_countries(
        geounit = "france", 
        type = "map_units", 
        scale = 10
      ) |> 
      st_as_sf() |>
      st_transform(crs = 2192) |>
      select(geounit, geometry)
    
  # ______________________________________________________________________________
  # Department / Region 
  } else if (which_region %in% c("dep", "reg")) {
    
    sf_france <-
      ne_states(geounit = "france") |>
      st_as_sf() |> 
      st_transform(crs = 2192) |>
      mutate(dep = str_extract(iso_3166_2, "\\d+"),
             dep_name = name,
             reg_name = region) |> 
      select(dep_name, dep, reg_name, geometry)
    
    if (which_region == "reg") {
      sf_france <- 
        sf_france |> 
        group_by(reg_name) %>% 
        summarize(geometry = st_union(geometry),
                  .groups = "keep") |> 
        ungroup()
    }
  
  # ______________________________________________________________________________
  # Sylvoecoregion / GRECO
  } else if (which_region %in% c("ser", "gre")) {
    
    # Function to give greco code associated name
    map_greco <- function(gre_code) {
      gre_mapping <- c(
        "A" = "Grand Ouest cristallin et océanique",
        "B" = "Centre Nord semi-océanique",
        "C" = "Grand Est semi-continental",
        "D" = "Vosges",
        "E" = "Jura",
        "F" = "Sud-Ouest océanique",
        "G" = "Massif central",
        "H" = "Alpes",
        "I" = "Pyrénées",
        "J" = "Méditerranée",
        "K" = "Corse"
      )
      
      description <- gre_mapping[gre_code]
      
      if (is.null(description)) {
        warning("Invalid GRE code")
        return(NA)
      }
      
      return(description)
    }
    
    # Load data
    sf_france <-
      sf::read_sf(here("data/raw/maps/France_shapefile/greco/France_ser_.shp")) |> 
      # sf::st_transform("WGS84") |> 
      st_transform(crs = 2192) |>
      rename(ser_num  = ser,
             ser      = codeser,
             gre_num  = codegreco,
             ser_name = NomSER) |> 
      mutate(
        ser      = as_factor(ser),
        ser_name = as_factor(ser_name),
        gre      = as.factor(substr(ser, 1, 1)),
        gre_name = map_greco(gre),
        gre_name = as.factor(gre_name)
        )
    
    if (which_region == "ser") {
      sf_france <- 
        sf_france |> 
        group_by(ser, ser_num, ser_name) |> 
        summarize(geometry = st_union(geometry),
                  .groups = "keep") |> 
        ungroup()
      
    } else if (which_region == "gre") {
      sf_france <- 
        sf_france |> 
        group_by(gre, gre_num, gre_name) |> 
        summarize(geometry = st_union(geometry),
                  .groups = "keep") |> 
        ungroup()
    }
    
  # ______________________________________________________________________________
  # Hexagons
  } else if (which_region == "hex") {
    
    message("> # polygons used per degree: ", n_poly)
    
    # Get shapefile extent
    sf_france <-  
      ne_countries(
      geounit = "france", 
      type = "map_units", 
      scale = 10
    ) |> 
    st_as_sf() |>
    st_transform(crs = 2192) |>
    select(geounit, geometry)
    
    # Make hexgrid over France
    grd <- 
      st_make_grid(
        sf_france,
        n = c(n_poly, n_poly),
        what = "polygons",
        square = FALSE
      )  |> 
      st_as_sf()  |>  # convert back to sf object
      rename(geometry = x) |> 
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
    
    # Drop all polygons over seas
    sf_france <- grd_clean |> drop_na()
    
  } 
  # ______________________________________________________________________________
  # Save file
  dir_tmp <- here("data/raw/maps/naturalearth/")
  if (!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = T, showWarnings = F)
  
  if (which_region == "hex") {
    filn <- paste0(dir_tmp, "natural_earth-shapefile_france_", which_region, n_poly, ".shp")
  } else {
    filn <- paste0(dir_tmp, "natural_earth-shapefile_france_", which_region, ".shp")
  }
  
  # st_write(sf_france, filn, quite = TRUE, delete_dsn = TRUE)
  
  # Return
  message("> CRD of shapefile is: ", st_crs(sf_france)$input)
  return(sf_france)
    
  # Debug plotting test
  sf_france |> 
    ggplot() +
    geom_sf()
  
}
