f_get_list_of_irrelevant_vars <- function() {
  
  # https://docs.google.com/spreadsheets/d/1MD-fXwS8ZCcWsOjwcMYLyM1Qef-T6uwf0OKVQjJjqJg/edit#gid=0""
  # Last updated: 2023-08-18
  vec <- c(
    "CIBLE",
    "CLON",
    "DDEC",
    "DECOUPE",
    "FORME",
    "HDEC",
    "HRB",
    "LFSD",
    "ORI",
    "Q1",
    "Q2",
    "Q3",
    "QUALITE",
    "R",
    "SIMPLIF",
    "TIGE",
    "ACCES",
    "ASPERITE",
    "AUTUT",
    "BOIS",
    "BPLANT",
    "DCESPAR1",
    "DCESPAR2",
    "DIST",
    "INTEGR",
    "IPLANT",
    "ITI",
    "PENTEXP",
    "PORTANCE",
    "PORTN",
    "TFORM",
    "TM2",
    "TPESPAR1",
    "TPESPAR2",
    "VIDEPEUPLIER",
    "VIDEPLANT"
  ) |> tolower()
  return(vec)
}
