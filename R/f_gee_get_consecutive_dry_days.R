gee_get_consecutive_dry_days <- function(
    roi,
    date_start,
    date_end
    ){
  
  # Code source: https://www.youtube.com/watch?v=ZOelcNKVcKA 
  
  image_col <- 
    ee$
    ImageCollection("NOAA/PERSIANN-CDR")$
    filterBounds(roi)$
    filterDate(date_start, date_end)
    
  image_init <-
    ee$
    Image$
    constant(0)$
    rename("precipitation")$
    cast(list("precipitation" = "long")) # In javascript, the list is a dictionary {... : ...}
  
  calc_dry_days <- function(current, previous){
    mask     <- current$remap(list(0), list(1), 0)
    last_img <- ee$Image(ee$List(previous)$get(-1))
    updated  <- last_img$add(mask)$multiply(mask)
    
    return(ee$List(previous)$add(updated))
  }
  
  img_tmp <- 
    ee$List(
      image_col$iterate(
        algorithm = calc_dry_days,
        first     = ee$List(list(image_init))
      )
    )
  
  coll_tmp  <- ee$ImageCollection(img_tmp)
  img_fin   <- coll_tmp$max()$toFloat()
  
  return(img_fin)
}
