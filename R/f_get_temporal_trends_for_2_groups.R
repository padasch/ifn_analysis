get_temporal_trends_for_2_groups <- function(
    df_in              = NULL,
    name_group_1       = "genus_lat",
    name_group_2       = "height_class",
    n_groups_1         = 3,
    n_groups_2         = 10,
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
      "-------------------------------------",
      "\n> get_temporal_trends_for_2_groups():",
      "\n  ...working on group: ", counter, " / ", length(most_common_lvls_g2)))
    
    df_loop <- df_tmp |> filter(group_2 %in% my_lvl)
    
    list_out[[my_lvl]] <-
      get_temporal_trends_for_1_group(
        df_in        = df_loop,
        name_group_1 = name_group_1,
        n_groups_1   = n_groups_1,
        prescribe_g1 = most_common_lvls_g1,
        min_trees_per_site = min_trees_per_site,
        min_sites_per_year = min_sites_per_year
      )
    
    # Extract data for each metric
    for (my_target in names(list_out[[my_lvl]])) {
      
      df_loop      <- list_out[[my_lvl]][[my_target]]$df_plot      |> mutate(group_2 = my_lvl, metric = my_target)
      df_loop_tree <- list_out[[my_lvl]][[my_target]]$df_treecount |> mutate(group_2 = my_lvl, metric = my_target)
      
      df_bothgroups <-  bind_rows(df_bothgroups, df_loop)
      df_treecount  <-  bind_rows(df_treecount, df_loop_tree)
    }
    
  }
  
  # Add treecount to group 1
  df_count_g1 <- 
    df_treecount |> 
    group_by(group_1, metric) |> 
    summarise(across(
      .cols = n, 
      .fns  = sum, na.rm = TRUE,
      .names = "n_g1"
    ))
  
  df_count_g2 <- 
    df_treecount |> 
    group_by(group_2, metric) |> 
    summarise(across(
      .cols = n, 
      .fns  = sum, na.rm = TRUE,
      .names = "n_g2"
    ))
  
  df_plot <-
    df_bothgroups |> 
    left_join(df_count_g1, by = join_by(group_1, metric)) |> 
    left_join(df_count_g2, by = join_by(group_2, metric)) |> 
    mutate(
      group_1_f = as.factor(paste0(group_1, " (N = ", n_g1, ")")),
      group_2_f = as.factor(paste0(group_2, " (N = ", n_g2, ")"))
      )
  
  # Fix height class order for group 1
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
  
  # Fix height class order for group 2
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
  
  for (my_target in unique(df_plot$metric)) {
    
    # Get ggplot labs
    my_labs  <- get_ggplot_labs(my_target)
    my_labs$caption <- paste0(
      paste0(my_labs$caption),
      "\nData was filtered for sites with at least ", min_trees_per_site, " trees per site.",
      "\nData was filtered for years with at least ", min_sites_per_year, " sites per year.",
      collapse = ""
    )
    
    df_i <- 
      df_plot |>
      filter(metric == my_target)
    
    plot_2groups <-
      df_i |>
      mutate(my_mean = ifelse(is.na(my_mean), -10, my_mean))  |> 
      ggplot(
        aes(
          x = campagne_1,
          y = my_mean,
          color = group_2_f,
          group = group_2_f,
        )
      ) +
      facet_wrap(~group_1_f) +
      geom_point() +
      geom_errorbar(
        aes(
          ymin = my_mean - my_se,
          ymax = my_mean + my_se
        ),
        # position = position_dodge(width = 0.2),
        # Adjust the width as needed
        width = 0.1  # Adjust the width of the error bars
      ) +
      geom_line() +
      theme_classic() +
      labs(
        title   = my_labs$title,
        caption = my_labs$caption,
        color   = name_group_2,
        x = "Year of First Census",
        y = my_labs$axis
      )
    
    # Set coloring 
    if (name_group_2 %in% c("height_class")) {
      plot_2groups <- 
        plot_2groups + 
        scale_color_viridis_d(end = 0.9, na.value = "white")
    } else {
      plot_2groups <- 
        plot_2groups + 
        scale_color_brewer(palette = "Paired")
    }
    
    # Set axis limits
    if ((df_i |> drop_na(my_mean) |> nrow()) == 0) {
      # Case when only NA values in the data
      plot_2groups <- plot_2groups + ylim(0, Inf)
    } else {
      
      # Case when not only NA data in the data
      my_ymax <- max(df_i$my_mean + df_i$my_se, na.rm = TRUE)
      my_ymin <- min(df_i$my_mean - df_i$my_se, na.rm = TRUE)
      
      plot_2groups <- 
        plot_2groups + 
        ylim(
          my_ymin,
          my_ymax + my_ymax*0.1
          )
      
      rm(list = c("my_ymax", "my_ymin"))
    }
    
    list_out[["plot_both_groups"]][[my_target]] <- plot_2groups
  }
  
  
  return(list_out)
}
