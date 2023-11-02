gee_get_landsat_task <- function(
    roi, 
    date_start, 
    date_end, 
    my_var, 
    function_to_apply){
  
  # ______________________________________________________________________________
  # Get functions
  # EVI
  getEVI <- function(image){
    
    # Compute the EVI using an expression.
    EVI <- image$expression(
      '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
      list(
        'NIR'  = image$select('SR_B5')$multiply(0.0000275)$add(-0.2), 
        'RED'  = image$select('SR_B4')$multiply(0.0000275)$add(-0.2),
        'BLUE' = image$select('SR_B2')$multiply(0.0000275)$add(-0.2)
      ))$rename("EVI")
    
    image <- image$addBands(EVI)
    
    return(image)
  }
  
  # NDVI
  getNDVI <- function(image){
    # Compute the NDVI using an expression.
    NDVI <- image$expression(
      '(NIR - RED) / (NIR + RED)',
      list(
        'NIR' = image$select('SR_B5')$multiply(0.0000275)$add(-0.2),
        'RED' = image$select('SR_B4')$multiply(0.0000275)$add(-0.2)
      ))$rename("NDVI")
    
    image <- image$addBands(NDVI)
    
    return(image)
  }
  
  # ______________________________________________________________________________
  # Get image
  image  <- 
    ee$
    ImageCollection("LANDSAT/LC08/C02/T1_L2")$
    filterDate(date_start, date_end)$
    filterBounds(roi)$
    filterMetadata("CLOUD_COVER", "less_than", 20)$
    map(getEVI)$
    map(getNDVI)
  
  image_fct <- gee_apply_function_to_img(image = image, which_fct  = function_to_apply)
  image_cli <- image_fct$clip(roi)
  
  # ______________________________________________________________________________
  # Return image
  if (my_var == "NDVI") {out <- image_cli$select("NDVI")}
  if (my_var == "EVI")  {out <- image_cli$select("EVI")}
  
  return(out)
}

# For visualisation:
# my_colors <- list(
#   "min" = -1,
#   "max" =  1,
#   "palette" = list("red", "white", "green")
# )
# Map$addLayer(out, my_colors, 'NDVI')
