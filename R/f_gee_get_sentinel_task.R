gee_get_sentinel_task <- function(
  roi, 
  date_start, 
  date_end, 
  my_var, 
  function_to_apply){
 
  # Basis of Code:
  # Cloud filter: https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_HARMONIZED
  # Get NVDI and EVI https://kaflekrishna.com.np/blog-detail/enhanced-vegetation-index-evi-sentinel-2-image-google-earth-engine/
  
  # ______________________________________________________________________________
  # Define Functions
  
  # Create mask to filter out cloudy pictures
  maskS2clouds <- function(image){
    qa <- image$select("QA60")
    
    # Bits 10 and 11 are clouds and cirrus, respectively.
    cloudBitMask  <-  bitwShiftL(1, 10)
    cirrusBitMask <-  bitwShiftL(1, 11)
    
    # Both flags should be set to zero, indicating clear conditions.
    mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
    
    return(image$updateMask(mask)$divide(10000))
  }
  
  
  # Add date property to images
  # Not really working but not so important, I think...
  # addDate <- function(image){
  #   
  #   img_date <- ee$Date(image$date())
  #   img_date <- ee$Number$parse(img_date$format('YYYYMMdd'))
  #   out <- image$addBands(ee$Image(img_date)$rename('system:time_start')$toInt())
  #   
  #   return(out)
  # }
  
  # EVI
  getEVI <- function(image){
    
    # Compute the EVI using an expression.
    # No need to divide by 10^5 again, because is done in cloud masking!
    EVI <- image$expression(
      '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
      list(
        'NIR' = image$select('B8'), 
        'RED' = image$select('B4'),
        'BLUE'= image$select('B2')
      ))$rename("EVI")
    
    image <- image$addBands(EVI)
    
    return(image)
  }
  
  # NDVI
  getNDVI <- function(image){
    # Compute the NDVI using an expression.
    # No need to divide by 10^5 again, because is done in cloud masking!
    EVI <- image$expression(
      '(NIR - RED) / (NIR + RED)',
      list(
        'NIR' = image$select('B8'),
        'RED' = image$select('B4')
      ))$rename("NDVI")
    
    image <- image$addBands(EVI)
    
    return(image)
  }
  
  # ______________________________________________________________________________
  # Load ImageCollection and apply functions
  # - Filter requested date range
  # - Filter requested area
  # - Filter for pixels with less than 20% cloud coverage
  # - 2nd cloud filter based on quality assurance information
  # - Calculate EVI
  # - Calculate NDVI
  # - Aggregate based on function of interest
  #   For some reason, people online prefer median instead of mean
  # - Clip to roi (not really sure what this does)
  
  sentinel  <- 
    ee$
    ImageCollection('COPERNICUS/S2_HARMONIZED')$
    filterDate(date_start, date_end)$
    filterBounds(roi)$
    filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
    map(maskS2clouds)$
    # map(addDate)$
    map(getEVI)$
    map(getNDVI)
  
  image_fct <- gee_apply_function_to_img(image = sentinel, which_fct  = function_to_apply)
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
