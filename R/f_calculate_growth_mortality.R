calculate_growth_mortality <- function(df_in) {
  
  nesting_vars <- df_in |> select(-data) |> names()
  message(
    "> calculate_growth_mortality():",
    "\n  ... calculating metrics of change at the inputed nesting level:",
    "\n  ... levels: ", paste0(nesting_vars, sep = ", ")
    )
  
  
  df_out <- 
    df_in |>
    mutate(
      n_ini = map_dbl(data, ~ nrow(.)),
      n_sur = map_dbl(data, ~ filter(., tree_state_change == "alive_alive") |> nrow()),

      n_fin = map_dbl(data, ~ filter(.,
                                     tree_state_change == "alive_alive" |
                                       tree_state_change == "new_alive") |> nrow()),
      
      ba_at_v1_of_alive_trees = map_dbl(data, ~ pull(., ba_1) |> sum()), # All trees in dataset were alive at v1
      
      ba_at_v2_of_alive_trees = map_dbl(data, ~ filter(., 
                                                       tree_state_change == "alive_alive" |
                                                         tree_state_change == "new_alive") |> 
                                          pull(ba_2) |> 
                                          sum()),
      
      ba_at_v1_of_surivors = map_dbl(data, ~ filter(., 
                                                    tree_state_change == "alive_alive") |>
                                       pull(ba_1) |> 
                                       sum()),
      
      ba_at_v2_of_survivors = map_dbl(data, ~ filter(., 
                                                     tree_state_change == "alive_alive") |>
                                        pull(ba_2) |> 
                                        sum()),
      
      ba_at_v2_of_dead = map_dbl(data, ~ filter(., 
                                                tree_state_change == "alive_dead") |>
                                   pull(ba_2) |> 
                                   sum()),
      
      # Mortality Esquivel
      n_mor_yr_esq  = (1 - (n_sur / n_ini) ^ (1 / 5)) * 100,
      
      # Mortality and Recruitment Hoshino
      n_mor_yr      = log(n_ini / n_sur) / 5 * 100,
      n_rec_yr      = log(n_fin / n_sur) / 5 * 100,
      
      # Loss, gain, ingrowth Hoshino
      ba_loss_yr    = log(ba_at_v1_of_alive_trees / ba_at_v1_of_surivors) / 5 * 100,
      ba_gain_yr    = log(ba_at_v2_of_alive_trees / ba_at_v1_of_surivors) / 5 * 100,
      ba_ingr_yr    = log(ba_at_v2_of_survivors   / ba_at_v1_of_surivors) / 5 * 100,
      
      # Isolated growth of basal area
      ba_growth_abs  = (ba_at_v2_of_alive_trees - ba_at_v1_of_surivors) / 5, # m2 tree / hectare / yr
      ba_growth_rate = ba_growth_abs / (ba_at_v1_of_surivors) * 100, # % rate
      
      # Isolated loss of basal area
      ba_loss_abs    = ba_at_v2_of_dead / 5, # m2 tree / hectare / yr
      ba_loss_rate   = ba_loss_abs   / (ba_at_v2_of_dead + ba_at_v2_of_survivors) * 100,  # % rate
      
      # Change in alive basal area
      ba_change_abs = ba_at_v2_of_alive_trees - ba_at_v1_of_alive_trees / 5,
      ba_change_rel = ba_change_abs / ba_at_v1_of_alive_trees / 5
    ) 
  
  if ("idp" %in% names(df_in$data[[1]])) {
    
    message("\n Site-level in nested data detected!",
            "\n Absolute rates get normalized by nr. of sites.")
    
    df_out <- 
      df_out |> 
      mutate(
        n_plots = map_dbl(data, ~ pull(., idp) |> unique() |> length()),
        ba_growth_abs = ba_growth_abs    / n_plots,  
        ba_loss_abs   = ba_loss_abs      / n_plots,
        ba_change_abs = ba_change_abs    / n_plots,
      )
  }
  
  return(df_out)
}
