f_get_fixed_vars_per_level <- function(level) {
  # Save some variable names for easier nesting
  
  # Levels:
  # location
  # location-tree
  # location-tree-campagne
  
  if (!(level %in% c("location", "location_tree", "location_tree_campaign"))) {
    stop("f_get_fixed_vars_per_level needs a level of: location, location_tree, location_tree_campaign")
  }
  
  vars_location      <- c("idp", "lon_fr", "lat_fr", "ser", "dep", "lat", "lon")
  
  vars_location_tree <- c(vars_location, 
                          "a", "tree_id", "visit_1", "visit_2", "revisit_state")
  
  vars_location_tree_campaign <- NA
  
  if (level == "location") { return(vars_location)}
  if (level == "location_tree") { return(vars_location_tree)}
  if (level == "location_tree_campaign") {cat("\n NOT IMPLEMENTED YET")}
  # if (level == "location_tree_campaign") { return(vars_location_tree_campaign)}
  
}
