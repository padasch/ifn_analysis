get_todays_file_directory <- function(
    my_dir) {
  
  dir_tmp <- here("output", my_dir, format(Sys.time(), format = "%Y-%m-%d"))
  if (!dir.exists(dir_tmp)) dir.create(dir_tmp, recursive = T, showWarnings = F)
  
  return(dir_tmp)
}