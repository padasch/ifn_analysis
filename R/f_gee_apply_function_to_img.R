gee_apply_function_to_img <- function(image, which_fct){
  
  # Quick handler to apply different functions to an gee image
  
  if (which_fct == "mean")   image <- image$mean()
  if (which_fct == "median") image <- image$median()
  if (which_fct == "max")    image <- image$max()
  if (which_fct == "min")    image <- image$min()
  
  return(image)
}
