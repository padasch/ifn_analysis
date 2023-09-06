# Classification of Tree Status 
# Primarily taken by VEGET Variable

# Last updated based on protocol: Monday, 2023-09-04

get_tree_state_dictionary <- function() {
  
  tibble::tribble(
    ~ign_code, ~tree_state, ~mode_of_death,
    "0"      , "alive"    , NA, 
    "1"      , "alive"    , NA, 
    "2"      , "dead"     , "fallen", 
    "5"      , "dead"     , "standing",     
    "A"      , "dead"     , "fallen",     
    "C"      , "dead"     , "standing",     
    "M"      , "dead"     , "standing",     
    "T"      , "dead"     , "fallen",     
    "6"      , "cut"      , NA,    
    "7"      , "cut"      , NA,    
    "N"      , NA         , NA, 
    "NA"     , NA         , NA, 
    "Z"      , "injured"  , NA
  )

}
