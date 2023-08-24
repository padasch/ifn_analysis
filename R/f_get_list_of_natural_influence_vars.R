f_get_list_of_natural_influence_vars <- function() {
  
  # https://docs.google.com/spreadsheets/d/1MD-fXwS8ZCcWsOjwcMYLyM1Qef-T6uwf0OKVQjJjqJg/edit#gid=0""
  # Last updated: 2023-08-24
  vec <- c(
    "ACCI",
    "DEGGIB",
    "SFCOEUR",
    "SFDORGE",
    "SFGELIV",
    "SFGUI",
    "SFPIED",
    "ANPYR",
    "DPYR",
    "INCID",
    "NINCID",
    "PBUIS"
  ) |> tolower()
  return(vec)
}
