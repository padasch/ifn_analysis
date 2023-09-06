
load_or_save_latest_file <- function(data_variable_name, action = "load") {
  
  require(here)
  
  data_dir <- here("data")
  tmp_dir <- here(data_dir, "tmp")
  
  if (action == "load") {
    # List all RDS files in the tmp directory that match the data_variable_name
    rds_files <- list.files(path = tmp_dir, pattern = paste0("_", data_variable_name), full.names = TRUE)
    
    if (length(rds_files) > 0) {
      # Extract modification times for all matching RDS files
      file_info <- lapply(rds_files, function(file) {
        info <- file.info(file)
        data.frame(File = file, LastModified = info$mtime)
      })
      
      # Combine the modification times into a single data frame
      file_info_df <- do.call(rbind, file_info)
      
      # Find the file with the latest modification time
      latest_file <- file_info_df[which.max(file_info_df$LastModified), "File"]
      
      # Read the latest matching RDS file
      assign(data_variable_name, readRDS(latest_file), envir = .GlobalEnv)
      
      # Display the latest modified file and its modification time
      cat("Loading the latest modified file:", latest_file, "\n")
      # cat("Last modified time:", max(file_info_df$LastModified), "\n")
    } else {
      cat("No matching RDS files found in the tmp directory.\n")
    }
  } else if (action == "save") {
    # Get the current date and time for the file name
    current_datetime <- Sys.time()
    formatted_datetime <- format(current_datetime, "%Y%m%d-%H%M%S")
    
    # Define the RDS file name using the input data_variable_name and creation date
    rds_file_name <- file.path(tmp_dir, paste0(formatted_datetime, "_", data_variable_name, ".rds"))
    
    # Save the object to an RDS file
    saveRDS(get(data_variable_name, envir = .GlobalEnv), file = rds_file_name)
    
    # Display the saved file name and time
    cat("Data saved to file:", rds_file_name, "\n")
  } else {
    cat("Invalid action. Use 'load' or 'save' as the action.\n")
  }
}
