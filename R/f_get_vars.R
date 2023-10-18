get_vars <- function(subset) {
  
  if (subset == "nature") {
    
    out <- 
      c("nincid_1", 
        "nincid_2", 
        "incid_1", 
        "incid_2",
        "sfgeliv_1", 
        "sfgeliv_2", 
        "sfgui_1", 
        "sfgui_2", 
        "sfdorge_1", 
        "sfdorge_2", 
        "mortb_1", 
        "mortb_2",
        "dpyr", 
        "deggib", 
        "sfcoeur", 
        "sfpied", 
        "acci"
        ) 
    
  } else if (subset == "human") {
    
    out <- 
      c(
        "def5", 
        "instp5", 
        "prelev5",
        "nlisi5",
        "andain", 
        "dc", 
        "elag", 
        "gest", 
        "tplant", 
        "tetard"
      )
    
  } else if (subset == "irrelevant") {
    out <- 
      c(
        "acces", 
        "asperite", 
        "autut", 
        "bois", 
        "bplant", 
        "dcespar1", 
        "dcespar2", 
        "dist", 
        "integr", 
        "iplant", 
        "iti", 
        "pentexp", 
        "portance", 
        "portn", 
        "tform", 
        "tm2", 
        "tpespar1", 
        "tpespar2", 
        "videpeuplier", 
        "videplant",
        "entp"
      )
  } else if (subset == "placette") {
    out <- 
      c(
        "acces", 
        "andain", 
        "anpyr", 
        "asperite", 
        "autut", 
        "bois", 
        "bord", 
        "bplant", 
        "cam", 
        "campagne", 
        "csa", 
        "cslisi", 
        "dc", 
        "dcespar1", 
        "dcespar2", 
        "def5", 
        "dep", 
        "dist", 
        "dpyr", 
        "elag", 
        "elisi", 
        "entp", 
        "gest", 
        "idp", 
        "incid", 
        "instp5", 
        "integr", 
        "iplant", 
        "iti", 
        "nincid", 
        "nlisi5", 
        "orniere", 
        "pbuis", 
        "pentexp", 
        "peupnr", 
        "plisi", 
        "portance", 
        "portn", 
        "prelev5", 
        "ser", 
        "sfo", 
        "sver", 
        "tcat10", 
        "tform", 
        "tm2", 
        "tpespar1", 
        "tpespar2", 
        "tplant", 
        "uta1", 
        "uta2", 
        "utip", 
        "videpeuplier", 
        "videplant", 
        "visite"
      )
    
  } else {
    stop("Non-valid subset input")
  }
  
  return(out)
}
