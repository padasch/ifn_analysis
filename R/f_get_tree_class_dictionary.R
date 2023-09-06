get_tree_class_dictionary <- function(l_raw_data, l_metadata) {
  
  # Wifi connection check
  if (!pingr::is_online()) {
    stop("⚠️ get_tree_class_dictionary() does not work without internet connection!\n")
  } 
  
  # Get all species in the tree dataset
  tree_species_french <- 
    l_raw_data$arbre$espar |> 
    unique()
  
  # Attach french names to levels in the tree data
  metadata_species_french <- 
    l_metadata$lvls |> 
    filter(
      str_detect(variable, "ESPAR"),
      lvl %in% tree_species_french) |> 
    mutate(
      label = ifelse(lvl == "29AF", "Autre feuillu", label),
      label = ifelse(lvl == "38AL", "Cytise des Alpes", label),
      label = ifelse(lvl == "49AM", "Aubépine monogyne", label),
      label = ifelse(lvl == "63"  , "Mélèze d'Europe", label),
      label = ifelse(lvl == "68CE", "Autre conifère", label)
    ) |> 
    select(lvl, label) |> 
    distinct() |> 
    rename(name_french = label,
           lvl_french  = lvl)
  
  # Get scientific species names and extract french names, if they were added
  # in brackets after the scientific name.
  metadata_species_scientific <- 
    l_metadata$lvls |> 
    filter(variable == "CDREF13") |> 
    select(lvl, label) |> 
    rename(name_scientific = label,
           lvl_scientific  = lvl) |> 
    mutate(name_french  = str_extract(name_scientific, "\\((.*?)\\)"),
           name_french = str_replace_all(name_french, "\\(|\\)", "")
    )
  
  # Join french and scientific names
  species_list <- 
    left_join(
      metadata_species_french, 
      metadata_species_scientific,
      by = join_by(name_french)
    )
  
  # Clean up names by hand because some french names could not be linked up
  # with scientific names
  species_list <- 
    species_list |> 
    mutate(
      name_scientific = ifelse(name_french == "Peuplier cultivé"           , "Populus", name_scientific), 
      name_scientific = ifelse(name_french == "Érable à feuilles d'obier"  , "Acer opalus", name_scientific), 
      name_scientific = ifelse(name_french == "Marronnier d'Inde"          , "Aesculus hippocastanum", name_scientific), 
      name_scientific = ifelse(name_french == "Filao"                      , "Casuarina equisetifolia", name_scientific), 
      name_scientific = ifelse(name_french == "Autre feuillu"              , "Magnoliopsida", name_scientific), 
      name_scientific = ifelse(name_french == "Eucalyptus (genre)"         , "Eucalyptus", name_scientific), 
      name_scientific = ifelse(name_french == "Pin noir d'Autriche"        , "Pinus nigra", name_scientific), 
      name_scientific = ifelse(name_french == "Pin brutia (ou) eldarica"   , "Pin brutia", name_scientific), 
      name_scientific = ifelse(name_french == "Mélèze d'Europe"            , "Larix decidua", name_scientific), 
      name_scientific = ifelse(name_french == "Cèdre de l'Atlas"           , "Cedrus atlantica", name_scientific), 
      name_scientific = ifelse(name_french == "Cèdre de Chypre"            , "Cedrus brevifolia", name_scientific), 
      name_scientific = ifelse(name_french == "Épicéa omorika"             , "Picea omorika", name_scientific), 
      name_scientific = ifelse(name_french == "Cyprès chauve"              , "Taxodium distichum", name_scientific), 
      name_scientific = ifelse(name_french == "Tsuga heterophylla"         , "Tsuga heterophylla", name_scientific), 
      name_scientific = ifelse(name_french == "Cryptomère du Japon"        , "Cryptomeria japonica", name_scientific), 
      name_scientific = ifelse(name_french == "Autre conifère"             , "Conifer", name_scientific), 
      name_scientific = ifelse(name_french == "Sapin d'Andalousie"         , "Abies pinsapo", name_scientific), 
      name_scientific = ifelse(name_french == "Pin à l'encens et hybrides" , "Pinus taeda", name_scientific), 
      name_scientific = ifelse(name_french == "Peuplier"                   , "Populus", name_scientific), 
      name_scientific = ifelse(name_french == "Mûrier blanc"               , "Morus alba", name_scientific), 
      name_scientific = ifelse(name_french == "Mûrier noir"                , "Morus nigra", name_scientific), 
      name_scientific = ifelse(name_french == "Vernis vrai"                , "Toxicodendron vernicifluum", name_scientific), 
      name_scientific = ifelse(name_french == "Aubépine azérolier"         , "Crataegus azarolus", name_scientific), 
      name_scientific = ifelse(name_french == "Oranger"                    , "Citrus sinensis", name_scientific), 
      name_scientific = ifelse(name_french == "Cerisier de Sainte Lucie"   , "Prunus mahaleb", name_scientific), 
      name_scientific = ifelse(name_french == "Tamaris d'Afrique"          , "Tamarix africana", name_scientific), 
      name_scientific = ifelse(name_french == "Prunelier"                  , "Prunus spinosa", name_scientific), 
      name_scientific = ifelse(name_french == "Fusain d'Europe"            , "Euonymus europaeus", name_scientific), 
      name_scientific = ifelse(name_french == "Sureau rouge"               , "Sambucus racemosa", name_scientific),
      name_scientific = ifelse(name_scientific == "Salix alba (Saule blanc)", "Salicaceae", name_scientific),
      name_scientific = ifelse(name_scientific == "Pin brutia"             , "Pinaceae", name_scientific)
    )
  
  # Run species list against online database of species and extract keep genus info
  species_df <- rgbif::name_backbone_checklist(species_list$name_scientific)
  
  # Attach information on tree type
  species_df <- 
    species_df |> 
    mutate(
      tree_class = ifelse(class == "Pinopsida", "pinus", "broadleaf"),
    ) |> 
    rename(name_scientific = verbatim_name)
  
  # Connect french_lvl with tree_class and clean up df
  out <- 
    inner_join(
      species_list, 
      species_df, 
      by = join_by(name_scientific), 
      relationship = "many-to-many"
    ) |> 
    select(lvl_french, tree_class) |> 
    distinct()
  
  return(out)
}
