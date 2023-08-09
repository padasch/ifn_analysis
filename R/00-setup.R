# R functions
sapply(
  list.files(here::here("R"), 
             pattern = "f_", 
             full.names = TRUE), 
  source
  )

# Packages
packages <- c(
  "here", "conflicted",
  "tidyverse", "ggplot2", "purrr", "ggplot2",
  "terra", "leaflet", "sp", "sf"
)

# Install packages not yet installed
installed_packages <- packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(packages[!installed_packages])
}

# Packages loading
invisible(lapply(packages, library, character.only = TRUE))
conflict_prefer(name = "select", winner = "dplyr")
conflict_prefer(name = "filter", winner = "dplyr")
