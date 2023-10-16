get_most_common_species <- function(df, level = NA){
  
  if (is.na(level)) {
    stop("One of the grouping levels is required: species, genus")
  }
  
  if (level == "genus") {
    out <- df |> group_by(genus_lat) |> summarise(n = n()) |> arrange(desc(n)) 
  } else if (condition) {
    out <- df |> group_by(species_lat) |> summarise(n = n()) |> arrange(desc(n))
  } 
  
  return(out)
}
