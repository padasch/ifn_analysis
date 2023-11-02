gee_get_era5_task <- function(
  my_data,
  roi, 
  date_start, 
  date_end, 
  my_var, 
  function_to_apply){
  
  image <- 
    ee$
    ImageCollection(my_data)$
    select(my_var)$
    filterBounds(roi)$
    filterDate(date_start,
               date_end)
  
  image <- gee_apply_function_to_img(image = image, which_fct  = function_to_apply)
  return(image)
}
