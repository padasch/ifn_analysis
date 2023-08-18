f_get_tree_index <- function(l_raw_data) {
  
  # Get location index
  idx_loc <- f_get_location_index(l_raw_data)
  
  # Add indicator on tree sampling
  df_tre_numerated <-
    l_raw_data$arbre |> 
    select(idp, campagne) |> 
    distinct() |> 
    arrange(idp, campagne) |> 
    group_by(idp) |> 
    mutate(visit = 1:n()) |> 
    ungroup()
  
  tree_index <- 
    df_tre_numerated |> 
    right_join(l_raw_data$arbre, by = c("idp", "campagne")) |> 
    select(idp, visit, a, campagne) |> 
    pivot_wider(
      names_from = visit, 
      values_from = campagne, 
      names_prefix = "visit_", 
      values_fill = NA) |> 
    mutate(
      revisit_state = NA,
      revisit_state = ifelse(!is.na(visit_1) & !is.na(visit_2), "revisited", revisit_state),
      revisit_state = ifelse(!is.na(visit_1) & is.na(visit_2),  "not_revisited", revisit_state),
      revisit_state = ifelse(is.na(visit_1)  & !is.na(visit_2), "newly_sampled", revisit_state),
      revisit_state = as.factor(revisit_state)
    )
  
  tree_index <- tree_index  |> mutate(tree_id = paste0(idp, "_", a))
  
  return(tree_index)
  
  # Commented out below, to not return nested df
  # # Final index
  # nesting_vars <- 
  #   left_join(tree_index, idx_loc,
  #             by = "idp") |> 
  #   select(-a, -revisit_state) |> 
  #   names()
  # 
  # final_index <- 
  #   left_join(tree_index, idx_loc, by = "idp") |> 
  #   group_by(idp, v1, v2, n_visits_tre, n_visits_loc, v_treedata) |> 
  #   nest() |> 
  #   rename(tree_index = data)
  
  # return(final_index)
}

