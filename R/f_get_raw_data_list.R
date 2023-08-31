f_get_raw_data_list <- function() {

  # Metadata List --------------------------------------------------------------
  l_metadata <- list()
  
  l_metadata[["metadata"]] <- 
    read_csv2(
      here::here("data/raw/export_dataifn_2005_2021/metadonnees.csv"), 
      show_col_types = FALSE,
      skip = 17,    # Skip initial descriptions
      n_max = 163,  # Only load variable descriptions
    ) |> 
    rename(variable = "// Code",
           label = "Libellé",
           description = "Définition") |> 
    arrange(variable)
  
  l_metadata[["units"]] <- 
    read_csv2(
      here::here("data/raw/export_dataifn_2005_2021/metadonnees.csv"), 
      show_col_types = FALSE,
      skip = 184,    # Skip initial descriptions and metadata
      n_max = 224,  # Only load unit descriptions
      
    ) |> 
    rename(variable = "// Donnée",
           label = "Libellé",
           unit = "Unité",
           campagnes = "Campagnes",
           type = "Type",
           description = "Définition") |> 
    arrange(variable)
  
  l_metadata[["lvls"]] <- 
    read_csv2(
      here::here("data/raw/export_dataifn_2005_2021/metadonnees.csv"), 
      show_col_types = FALSE,
      skip = 412,    # Skip metadata and units
    ) |> 
    rename(variable = "// Unité",
           label = "Libellé",
           lvl = "Code",
           description = "Définition") |> 
    arrange(variable)
  
  # Load all datafiles into a list ---------------------------------------------
  l_raw_data <- list()
  
  # Data directory
  dr_data <- here::here("data/raw/export_dataifn_2005_2021/")
  
  for (file in list.files(dr_data, pattern = ".csv", include.dirs = FALSE)) {
    
    # Get name and path for data
    name <- file |> stringr::str_to_lower() |> stringr::str_remove(".csv")
    path <- paste0(dr_data, file)
    
    # Skip non-data files
    if (name %in% c("metadonnees", "espar-cdref13")) {next}
    
    # Save data to list
    l_raw_data[[name]] <-
      read_delim(
        path, 
        delim = ";", 
        escape_double = FALSE,
        trim_ws = TRUE,
        show_col_types = FALSE,
        col_types = cols(XL = col_character(),  # Load coordinates as char
                         YL = col_character())  # to avoid automatic rounding
      )
    
    # Remove columns that only hold NA's
    l_raw_data[[name]] <- l_raw_data[[name]] |> select(where(~!all(is.na(.))))
  }
  
  # Clean up variable names
  l_raw_data <- purrr::map(l_raw_data, ~format_vars(l_metadata$units, .))
  
  # Drop columns with NA only
  l_raw_data <- purrr::map(l_raw_data, ~. |> select(where(~!all(is.na(.)))))
  
  # Rearrange some variables by hand
  l_raw_data$arbre <- 
    l_raw_data$arbre |> 
    relocate(idp, campagne, a, c13, veget, veget5)
  
  l_raw_data$placette <- 
    l_raw_data$placette |> 
    relocate(idp, campagne, visite, incid, prelev5, def5, gest, elag)
  
  return(list(l_raw_data, l_metadata))
  }
