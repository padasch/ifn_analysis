# Load QGIS API
library(qgisprocess)

# Enable plugins
qgis_enable_plugins("otbprovider")

# General help
# Find all functions: qgis_algorithms()
# Find specific function: qgis_algorithms() |> filter(str_detect(algorithm, "XXX")) |> pull(algorithm)
# Find help on a specific function: qgis_show_help("native:buffer")
# get more info: qgis_get_description("grass7:r.slope.aspect")

# Load original 25m DEM from merging individual files in QGIS (could be automated here too)
dem25m <- terra::rast(here("data/raw/dem_france/derivatives/france_25m_altitude_dem.tif")) 

all_dem <- list("25" = dem25m)
all_res <- c(25, 500, 1000)

# Upscale DEM to 500m and 1000m resolutions
# qgis_show_help("gdal:warpreproject")

for (my_res in all_res) {
  
  if (my_res != 25) {
    
    all_dem[[as.character(my_res)]] <- 
      qgis_run_algorithm(
        "gdal:warpreproject",
        INPUT             = dem25m,
        RESAMPLING        = 1, # use bi-linear re-sampling
        TARGET_RESOLUTION = my_res
      ) |> 
      qgis_as_raster()
    
    crs(all_dem[[as.character(my_res)]]) <- 'EPSG:2154'
    
    my_file <- paste0(
      here("data/raw/dem_france/derivatives"),
      "/france_", my_res, "m_altitude_dem.tif"
    )
    
    writeRaster(all_dem[[as.character(my_res)]], my_file, overwrite=TRUE)
  }
  
  message("\n> working on resolution: ", my_res)
  # ______________________________________________________________________________
  # Calculate Slope
  message("\n  ...working on: Calculate Slope")
  my_output <- 
    qgis_run_algorithm("gdal:slope", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "m_slope.tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
  
  # ______________________________________________________________________________
  # Calculate hillshade
  message("\n  ...working on: Calculate hillshade")
  my_output <- 
    qgis_run_algorithm("gdal:hillshade", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "m_hillshade.tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
  
  # ______________________________________________________________________________
  # Calculate roughness
  message("\n  ...working on: Calculate roughness")
  my_output <- 
    qgis_run_algorithm("gdal:roughness", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "m_roughness", ".tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
  
  # ______________________________________________________________________________
  # Terrain Ruggedness Index
  message("\n  ...working on: Terrain Ruggedness Index")
  my_output <- 
    qgis_run_algorithm("gdal:triterrainruggednessindex", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "tri", ".tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
  
  # ______________________________________________________________________________
  # Topographic Position Index
  message("\n  ...working on: Topographic Position Index")
  my_output <- 
    qgis_run_algorithm("gdal:triterrainruggednessindex", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "tpi", ".tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
  
  # ______________________________________________________________________________
  # Aspect
  message("\n  ...working on: Aspect")
  my_output <- 
    qgis_run_algorithm("gdal:aspect", INPUT = all_dem[[as.character(my_res)]]) |> 
    qgis_as_raster()
  
  my_file <- paste0(here("data/raw/dem_france/derivatives"), "/france_", my_res, "aspect", ".tif")
  writeRaster(my_output, my_file, overwrite=TRUE)
}

