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
    rename(species_fr = label,
           lvl_french  = lvl)
  
  # Get scientific species names and extract french names, if they were added
  # in brackets after the scientific name.
  metadata_species_scientific <- 
    l_metadata$lvls |> 
    filter(variable == "CDREF13") |> 
    select(lvl, label) |> 
    rename(species_lat = label,
           lvl_scientific  = lvl) |> 
    mutate(species_fr  = str_extract(species_lat, "\\((.*?)\\)"),
           species_fr = str_replace_all(species_fr, "\\(|\\)", "")
    )
  
  # Join french and scientific names
  species_list <- 
    left_join(
      metadata_species_french, 
      metadata_species_scientific,
      by = join_by(species_fr)
    )
  
  # Clean up names by hand because some french names could not be linked up
  # with scientific names
  species_list <- 
    species_list |> 
    mutate(
      species_lat = ifelse(species_fr == "Peuplier cultivé"           , "Populus", species_lat), 
      species_lat = ifelse(species_fr == "Érable à feuilles d'obier"  , "Acer opalus", species_lat), 
      species_lat = ifelse(species_fr == "Marronnier d'Inde"          , "Aesculus hippocastanum", species_lat), 
      species_lat = ifelse(species_fr == "Filao"                      , "Casuarina equisetifolia", species_lat), 
      species_lat = ifelse(species_fr == "Autre feuillu"              , "Magnoliopsida", species_lat), 
      species_lat = ifelse(species_fr == "Eucalyptus (genre)"         , "Eucalyptus", species_lat), 
      species_lat = ifelse(species_fr == "Pin noir d'Autriche"        , "Pinus nigra", species_lat), 
      species_lat = ifelse(species_fr == "Pin brutia (ou) eldarica"   , "Pin brutia", species_lat), 
      species_lat = ifelse(species_fr == "Mélèze d'Europe"            , "Larix decidua", species_lat), 
      species_lat = ifelse(species_fr == "Cèdre de l'Atlas"           , "Cedrus atlantica", species_lat), 
      species_lat = ifelse(species_fr == "Cèdre de Chypre"            , "Cedrus brevifolia", species_lat), 
      species_lat = ifelse(species_fr == "Épicéa omorika"             , "Picea omorika", species_lat), 
      species_lat = ifelse(species_fr == "Cyprès chauve"              , "Taxodium distichum", species_lat), 
      species_lat = ifelse(species_fr == "Tsuga heterophylla"         , "Tsuga heterophylla", species_lat), 
      species_lat = ifelse(species_fr == "Cryptomère du Japon"        , "Cryptomeria japonica", species_lat), 
      species_lat = ifelse(species_fr == "Autre conifère"             , "Conifer", species_lat), 
      species_lat = ifelse(species_fr == "Sapin d'Andalousie"         , "Abies pinsapo", species_lat), 
      species_lat = ifelse(species_fr == "Pin à l'encens et hybrides" , "Pinus taeda", species_lat), 
      species_lat = ifelse(species_fr == "Peuplier"                   , "Populus", species_lat), 
      species_lat = ifelse(species_fr == "Mûrier blanc"               , "Morus alba", species_lat), 
      species_lat = ifelse(species_fr == "Mûrier noir"                , "Morus nigra", species_lat), 
      species_lat = ifelse(species_fr == "Vernis vrai"                , "Toxicodendron vernicifluum", species_lat), 
      species_lat = ifelse(species_fr == "Aubépine azérolier"         , "Crataegus azarolus", species_lat), 
      species_lat = ifelse(species_fr == "Oranger"                    , "Citrus sinensis", species_lat), 
      species_lat = ifelse(species_fr == "Cerisier de Sainte Lucie"   , "Prunus mahaleb", species_lat), 
      species_lat = ifelse(species_fr == "Tamaris d'Afrique"          , "Tamarix africana", species_lat), 
      species_lat = ifelse(species_fr == "Prunelier"                  , "Prunus spinosa", species_lat), 
      species_lat = ifelse(species_fr == "Fusain d'Europe"            , "Euonymus europaeus", species_lat), 
      species_lat = ifelse(species_fr == "Sureau rouge"               , "Sambucus racemosa", species_lat),
      species_lat = ifelse(species_lat == "Salix alba (Saule blanc)"  , "Salicaceae", species_lat),
      species_lat = ifelse(species_lat == "Pin brutia"                , "Pinaceae", species_lat)
    )
  
  # Run species list against online database of species and extract keep genus info
  species_df <- rgbif::name_backbone_checklist(species_list$species_lat)
  
  # Attach information on tree type
  species_df <- 
    species_df |> 
    mutate(
      tree_class = ifelse(class == "Pinopsida", "pinus", "broadleaf"),
    ) |> 
    rename(species_lat = verbatim_name)
  
  # Connect french_lvl with tree_class and clean up df
  out <- 
    inner_join(
      species_list, 
      species_df, 
      by = join_by(species_lat), 
      relationship = "many-to-many"
    ) |> 
    rename(
      genus_lat  = genus,
      family_lat = family,
      order_lat  = order
    ) |> 
    select(lvl_french, tree_class, species_lat, genus_lat, family_lat, order_lat) |> 
    distinct()
  
  return(out)
}
