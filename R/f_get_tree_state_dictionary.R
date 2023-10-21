# Classification of Tree Status 
# Primarily taken by VEGET Variable

# Last updated based on protocol: Monday, 2023-09-04

get_tree_state_dictionary <- function() {
  
  tibble::tribble(
    ~ign_code, ~tree_state, ~alive_but_injured, ~mode_of_death,
    "0"      , "alive"    , "no",  NA, 
    "1"      , "alive"    , "no",  NA, 
    "2"      , "dead"     , "no",  "fallen", 
    "5"      , "dead"     , "no",  "standing",     
    "A"      , "dead"     , "no",  "fallen",     
    "C"      , "dead"     , "no",  "standing",     
    "M"      , "dead"     , "no",  "standing",     
    "T"      , "dead"     , "no",  "fallen",     
    "6"      , "cut"      , "no",  NA,    
    "7"      , "cut"      , "no",  NA,    
    "N"      , NA         , "no",  NA, 
    "NA"     , NA         , "no",  NA, 
    "Z"      , "alive"    , "yes", NA
  )

}
