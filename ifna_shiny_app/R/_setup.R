# # Packages
# packages <- c(
#   
#   # Workspace management
#   "here", "conflicted", "shiny",
#   
#   # HTML Output
#   "DT", # datatable for interactive tables
#   
#   # Parallel computing
#   # "multidplyr", # Parallel computing, keep before tidyverse!
#   # "furrr", # Parallel computing
#   
#   # Data Wrangling
#   "tidyverse", "purrr",
#   
#   # Plotting
#   "ggplot2", "ggridges", "patchwork", "Cairo",
#   
#   # Spatial data
#   "terra", "leaflet", "sp", "sf", "raster",
#   
#   # To facilitate coding:
#   "tictoc", "beepr"
# )
# 
# # install.packages("here")
# 
# # R functions
# sapply(
#   list.files("R", 
#              pattern = "f_", 
#              full.names = TRUE), 
#   source
# )
# 
# 
# # Install packages not yet installed
# installed_packages <- packages %in% rownames(installed.packages())
# if (any(installed_packages == FALSE)) {
#   install.packages(packages[!installed_packages])
# }
# 
# ## Specifying parallel computing
# # plan (multisession, workers = 9)
# 
# # library(multidplyr)
# # library(dplyr, warn.conflicts = FALSE)
# # 
# # cluster <- new_cluster(parallel::detectCores() - 2) # Use all but two cores
# # cluster_library(cluster, "dplyr")
# 
# # Packages loading
# invisible(lapply(packages, library, character.only = TRUE))
# conflict_prefer(name = "select", winner = "dplyr")
# conflict_prefer(name = "filter", winner = "dplyr")
# 
# 
