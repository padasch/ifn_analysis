gee_get_modis_task <- function(
  my_data,
  roi, 
  date_start, 
  date_end, 
  my_var, 
  function_to_apply){
  
  
  if (my_var == "LST_Day_1km") {
  # Land Surface Temperature ----
  # ______________________________________________________________________________
    
    # Function: Quality Control
    modis_qc_mask <- function(image){
      
      qa <- image$select("QC_Day")
      qa_bit_mask  <-  bitwShiftL(1, 0) # QA in bit 0 and 1
      mask <- qa$bitwiseAnd(qa_bit_mask)$eq(0) # Bit should be 0, indicating good QC
      
      return(image$updateMask(mask))
    }
    
    # Function: Turn scaled Kevlin into ready-to-use degrees Celsius
    units_to_degC <- function(image){return(image$multiply(0.02)$subtract(273.15))}
    
    # Get image
    image  <- 
      ee$
      ImageCollection("MODIS/061/MOD11A1")$
      filterDate(date_start, date_end)$
      filterBounds(roi)$
      map(modis_qc_mask)$
      map(units_to_degC)
    
    image_fct <- gee_apply_function_to_img(image = image, which_fct  = function_to_apply)
    image_cli <- image_fct$clip(roi)
    out       <- image_cli$select("LST_Day_1km")
    
    return(out)
    
    
  # ______________________________________________________________________________
  # Vegetation Indeces
  } else if (my_var %in% c("EVI", "NDVI")) {
    
    # Function: Quality Control
    modis_qc_mask <- function(image){
      
      qa <- image$select("DetailedQA")
      qa_bit_mask  <-  bitwShiftL(1, 0) # QA in bit 0 and 1
      mask <- qa$bitwiseAnd(qa_bit_mask)$eq(0) # Bit should be 0, indicating good QC
      
      return(image$updateMask(mask)$divide(10000))
    }
    
    # Get image
    image  <- 
      ee$
      ImageCollection("MODIS/061/MOD13A1")$
      filterDate(date_start, date_end)$
      filterBounds(roi)$
      map(modis_qc_mask)
    
    image_fct <- gee_apply_function_to_img(image = image, which_fct  = function_to_apply)
    image_cli <- image_fct$clip(roi)
    
    if (my_var == "NDVI") {out <- image_cli$select("NDVI")}
    if (my_var == "EVI")  {out <- image_cli$select("EVI")}
    
    return(out)
    
  } else {
    stop("Invalid variable selected")
  }
}

# For visualisation:
# my_colors <- list(
#   "min" = -1,
#   "max" =  1,
#   "palette" = list("red", "orange", "green")
# )
# 
# my_colors <- list(
#   "min" = -10,
#   "max" =  40,
#   "palette" = list("blue", "white", "red")
# )
# 
# Map$centerObject(roi)
# Map$addLayer(out, my_colors, 'My Layer')
# Map$addLayer(out2, my_colors, 'My Layer 2')
