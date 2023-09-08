# Classification of Tree Status 
# Primarily taken by VEGET Variable

# Last updated based on protocol: Monday, 2023-09-04

get_tree_state_dictionary <- function() {
  
  tibble::tribble(
    ~ign_code, ~tree_state, ~alive_but_injured, ~mode_of_death,
    "0"      , "alive"    , FALSE, NA, 
    "1"      , "alive"    , FALSE, NA, 
    "2"      , "dead"     , FALSE, "fallen", 
    "5"      , "dead"     , FALSE, "standing",     
    "A"      , "dead"     , FALSE, "fallen",     
    "C"      , "dead"     , FALSE, "standing",     
    "M"      , "dead"     , FALSE, "standing",     
    "T"      , "dead"     , FALSE, "fallen",     
    "6"      , "cut"      , FALSE, NA,    
    "7"      , "cut"      , FALSE, NA,    
    "N"      , NA         , FALSE, NA, 
    "NA"     , NA         , FALSE, NA, 
    "Z"      , "alive"    , TRUE, NA
  )

}
