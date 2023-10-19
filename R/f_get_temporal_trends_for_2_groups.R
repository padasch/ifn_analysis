get_temporal_trends_for_2_groups <- function(
    df_in              = NULL,
    name_group_1       = "genus_lat",
    name_group_2       = "height_class",
    n_groups_1         = 3,
    n_groups_2         = 10,
    my_target          = "ba_loss_rate",
    min_trees_per_site = 3,
    min_sites_per_year = 5
) {
  
  # ______________________________________________________________________________
  # Inputs
  # Get most common lvls of group 1
  most_common_lvls_g1 <- 
    df_in[[name_group_1]] |> 
    table() |> 
    sort(decreasing = TRUE) |> 
    head(n_groups_1) |> 
    names()
  
  # Get most common lvls of group 2
  most_common_lvls_g2 <- 
    df_in[[name_group_2]] |> 
    table() |> 
    sort(decreasing = TRUE) |> 
    head(n_groups_2) |> 
    names()
  
  # Wrangle input data
  df_tmp <- 
    df_in |> 
    mutate(
      group_1 = get(name_group_1),
      group_2 = get(name_group_2)
    ) |> 
    filter(   # Filter only for entries in most common lvls
      group_1 %in% most_common_lvls_g1,
      group_2 %in% most_common_lvls_g2
    ) |> 
    drop_na(   # Remove where grouping information is NA
      group_1,
      group_2) 
  
  # ______________________________________________________________________________
  # Loop
  # Get variables
  counter <- 0
  df_bothgroups <- tibble()
  df_treecount <- tibble()
  list_out <- list()
  
  for (my_lvl in most_common_lvls_g2) {
    
    counter <- counter + 1
    
    message(paste0(
      "\014",
      "> get_temporal_trends_for_2_groups():",
      "\n  ...working on group: ", counter, " / ", length(most_common_lvls_g2)))
    
    df_loop <- df_tmp |> filter(group_2 %in% my_lvl)
    
    list_out[[my_lvl]] <-
      get_temporal_trends_for_1_group(
        df_in        = df_loop,
        name_group_1 = name_group_1,
        n_groups_1   = n_groups_1,
        prescribe_g1 = most_common_lvls_g1,
        my_target    = my_target,
        min_trees_per_site = min_trees_per_site,
        min_sites_per_year = min_sites_per_year
      )
    
    df_loop <- list_out[[my_lvl]]$df_plot |> mutate(group_2 = my_lvl)
    df_loop_tree <- list_out[[my_lvl]]$df_treecount |> mutate(group_2 = my_lvl)
    
    df_bothgroups <-  bind_rows(df_bothgroups, df_loop)
    df_treecount  <-  bind_rows(df_treecount, df_loop_tree)
  }
  
  # Add treecount to group 1
  df_count_g1 <- 
    df_treecount |> 
    group_by(group_1) |> 
    summarise(across(
      .cols = n, 
      .fns  = sum, na.rm = TRUE,
      .names = "n_g1"
    ))
  
  df_count_g2 <- 
    df_treecount |> 
    group_by(group_2) |> 
    summarise(across(
      .cols = n, 
      .fns  = sum, na.rm = TRUE,
      .names = "n_g2"
    ))
  
  df_plot <-
    df_bothgroups |> 
    left_join(df_count_g1, by = join_by(group_1)) |> 
    left_join(df_count_g2, by = join_by(group_2)) |> 
    mutate(
      group_1_f = as.factor(paste0(group_1, " (N = ", n_g1, ")")),
      group_2_f = as.factor(paste0(group_2, " (N = ", n_g2, ")"))
      )
  
  if (name_group_1 == "height_class") {
    all_levels <- levels(df_plot$group_1_f)
    position_5m <- grep("<5m", all_levels)
    
    if (length(position_5m) != 0) {
      # Reorder levels with "<5m" as the first level
      df_plot$group_1_f <- 
        factor(df_plot$group_1_f, 
               levels = c(all_levels[position_5m], setdiff(all_levels, all_levels[position_5m])))
    }
  }
  
  if (name_group_2 == "height_class") {
    all_levels <- levels(df_plot$group_2_f)
    position_5m <- grep("<5m", all_levels)
    
    if (length(position_5m) != 0) {
      # Reorder levels with "<5m" as the first level
      df_plot$group_2_f <- 
        factor(df_plot$group_2_f, 
               levels = c(all_levels[position_5m], setdiff(all_levels, all_levels[position_5m])))
    }
  }
  
  
  # ______________________________________________________________________________
  # Plot
  
  var_mean <- paste0(my_target, "_mean")
  var_se   <- paste0(my_target, "_se")
  
  plot_2groups <- 
    df_plot |> 
    ggplot(
      aes(
        x = campagne_1,
        y = get(var_mean),
        color = group_2_f,
        group = group_2_f,
      )
    ) +
    facet_wrap(~group_1_f) +
    geom_point() +
    geom_errorbar(
      aes(
        ymin = get(var_mean) - get(var_se),
        ymax = get(var_mean) + get(var_se)
      ),
      # position = position_dodge(width = 0.2),
      # Adjust the width as needed
      width = 0.1  # Adjust the width of the error bars
    ) +
    geom_line() +
    theme_classic() +
    labs(
      # title   = txt_title,
      # caption = txt_caption,
      color = name_group_2,
      x = "Year of First Census",
      y = my_target
    ) + 
    scale_color_viridis_d(end = 0.9)
  
  list_out[["both_groups"]] <- plot_2groups
  
  return(list_out)
}
