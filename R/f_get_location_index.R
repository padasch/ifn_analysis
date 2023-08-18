#' Function to get reduced index on sampling information on location lvl
#'
#' @param l_raw_data must be the list of raw data containing $arbre and $placette
#'
#' @return
#' @export
#'
#' @examples
f_get_location_index <- function(l_raw_data) {
  
  # Get index for counting amount of visits (= is there one or two visits available?)
  f_get_n_visits <- function(df_in){
    
    #' Subfunction to count visits per location
    
    df_out <- 
      df_in |> 
      select(idp, campagne) |> 
      distinct() |> 
      group_by(idp) |> 
      mutate(n_visits = n()) |> 
      ungroup() |> 
      select(-campagne) |> 
      distinct()
    
    return(df_out)
  }
  
  idx_n_visits_loc <- f_get_n_visits(l_raw_data$placette)
  idx_n_visits_tre <- f_get_n_visits(l_raw_data$arbre)
  
  idx_counts <- full_join(
    idx_n_visits_tre |> rename(n_visits_tre = n_visits), 
    idx_n_visits_loc |> rename(n_visits_loc = n_visits),
    by = "idp"
  )
  
  # Get index with visit number stating repetition of visit
  idx_visite <- 
    l_raw_data$placette |> 
    select(idp, campagne, visite) |> 
    mutate(visite = ifelse(visite == 1, "v1", "v2")) |> 
    pivot_wider(values_from = c(campagne), names_from = c(visite))
  
  # Combine indexes
  idx_comb <- left_join(idx_visite, idx_counts, by = "idp") |> arrange(idp)
  
  # Attach indicator from which year tree data is from
  vec_tmp <- idx_comb |> filter(n_visits_tre == 1) |> pull(idp)
  
  df_yr <- 
    l_raw_data$arbre |> 
    select(idp, campagne) |> 
    distinct() |> 
    filter(idp %in% vec_tmp)
  
  final_index <- 
    idx_comb |> 
    left_join(df_yr, by = "idp") |> 
    mutate(
      v_treedata = NA,
      v_treedata = ifelse(n_visits_tre == 2, NA, v_treedata),
      v_treedata = ifelse(n_visits_tre == 1 & is.na(v1), "v2", v_treedata),
      v_treedata = ifelse(n_visits_tre == 1 & is.na(v2), "v1", v_treedata),
      v_treedata = ifelse((!is.na(v1) & !is.na(v2)) & v1 == campagne, "v1", v_treedata),
      v_treedata = ifelse((!is.na(v1) & !is.na(v2)) & v2 == campagne, "v2", v_treedata)
      ) |> 
    select(-campagne)

  # Return output
  return(final_index)
}

