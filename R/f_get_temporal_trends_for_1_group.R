get_temporal_trends_for_1_group <- function(
    df_in              = NULL,
    name_group_1       = NULL,
    n_groups_1         = 9,
    prescribe_g1       = NULL,
    min_trees_per_site = NULL,
    min_sites_per_year = NULL
) {
  
  # Reference inputs
  # df_in: Input dataframe, usually just final nfi dataset
  # group_1: Outter group for which grouping should be done (for double-plot, this is the facet)
  # n_groups_1: The n most common levels of group_1 (set to 9 by default for nice facet)
  # prescribe_g1: Prescribed list of factor levels for which g1 should be filtered for
  # min_trees_per_site: The minimum number of trees that need to be in a site to calculate that
  #   site's metrics of change.
  # min_sites_per_year: What is the minimum number of sites that must be observed per year?
  
  
  if (is.null(df_in)) stop("Missing input for: df_in")
  if (is.null(name_group_1)) stop("Missing input for: name_group_1")
  if (is.null(n_groups_1)) stop("Missing input for: n_groups_1")
  # if(is.null(prescribe_g1)) stop("Missing input for: prescribe_g1")
  if (is.null(min_trees_per_site)) stop("Missing input for: min_trees_per_site")
  if (is.null(min_sites_per_year)) stop("Missing input for: min_sites_per_year")
  
  # ______________________________________________________________________________
  # Set Variables
  # Potentially to be moved to input:
  min_census_counts <- 5 # Number of census counts needed to calculate trend (if based on means)
  min_census_counts_for_plot <- 5  # Number of data points needed to be shown on map
  
  # Define base grouping
  # Important: Keep idp at first position here!
  my_grouping        <- c("idp", "campagne_1", "group_1")
  
  if (is.null(prescribe_g1)) {
    most_common_lvls   <-
      df_in[[name_group_1]] |>
      table() |>
      sort(decreasing = TRUE) |>
      head(n_groups_1) |> 
      names()
  } else {
    most_common_lvls <- prescribe_g1
  }

  df_tmp <- 
    df_in |> 
    mutate(group_1 = get(name_group_1)) |> 
    filter(group_1 %in% most_common_lvls) |> 
    drop_na(group_1) # Remove where grouping information is NA
  
  # ______________________________________________________________________________
  # Calculate change per group
  # - Data needs to be nested by plot/year/species or height or both
  
  message("> get_temporal_trends_for_1_group():")
  message("  ...filtering sites and years with enough data.")
  
  df_nested_site_level <-
    df_tmp |> 
    select(               # Select only relevant variables to speed up computation
      idp,
      campagne_1,
      group_1,
      tree_state_change,
      ba_2,
      ba_1
    ) |>
    group_by_at(my_grouping) |> 
    filter(n() >= min_trees_per_site) |> 
    nest()
  
  sites_to_keep <- 
    df_nested_site_level |> 
    ungroup() |> 
    select(-data) |> 
    group_by(campagne_1, group_1) |> 
    nest() |> 
    mutate(n = map_dbl(data, ~nrow(.))) |> 
    filter(n > min_sites_per_year) |> 
    unnest(data) |> 
    ungroup() |> 
    pull(idp)
  
  df_nested <- df_nested_site_level |> filter(idp %in% sites_to_keep)
  
  tic()
  df_change <- 
    df_nested |> 
    calculate_growth_mortality() |> 
    ungroup()
  toc()
  # beep()
  
  # Summarise metrics of change
  vars_to_summarise <- df_change |> select(where(is.numeric), -campagne_1) |> names()
  
  df_summarised <- 
    df_change |> 
    group_by_at(my_grouping[-1]) |> 
    mutate(count = n()) |>  # Counting the number of entries per site-year-group1 group
    summarise(across(
      .cols = all_of(vars_to_summarise), 
      .fns  = list(mean = mean, se = std_error), na.rm = TRUE, 
      .names = "{col}_{fn}"
    ))
  
  # ______________________________________________________________________________
  # START LOOP OVER TARGET VARIABLES
  # ______________________________________________________________________________
  
  list_out <- list()
  df_out   <- tibble()
  all_vars <- get_ggplot_labs(return_available_vars = TRUE)
  
  for (my_target in all_vars) {
    
    # Clear all loop variables (probably not needed but ensures no variable carry-over)
    if (my_target != all_vars[1]) {
      rm(
        list = c("df_plot", "tree_count_info", "df_stats", 
                 "plot_allinone", "plot_facet", "my_labs",
                 "my_ymin", "my_ymax"
                 )
      )
    }
    
    # Target-based variables
    var_mean <- paste0(my_target, "_mean")
    var_se   <- paste0(my_target, "_se")
    
    if (!(var_mean %in% names(df_summarised))) {
      message(" ... ⚠️ Target variable was not calculated and thus skipped: ", my_target)
      next()
    }
    
    message(" ... working on ", my_target)
    
    list_out[[my_target]] <- list()
    df_loop <- tibble(target = my_target)
    
    # Get ggplot labs
    my_labs  <- get_ggplot_labs(my_target)
    my_labs$caption <- paste0(
      paste0(my_labs$caption),
      "\nData was filtered for sites with at least ", min_trees_per_site, " trees per site.",
      "\nData was filtered for years with at least ", min_sites_per_year, " sites per year.",
      collapse = ""
    )
    
    # ______________________________________________________________________________
    # └ Trend Statistics ----
    #   TODO: Question, do we analyse the trend of the means or do we analyse the trend over all
    #   sites at once? Trend over means has less data and therefore less significance signals.
    #   For example, the trend in Fraxinus should be picked up. But it is not when analysing the
    #   trend over the mean values.
    
    # Trend over all sites at once:
    # df_stats <- 
    #   df_change |> 
    #   select(-data) |> 
    #   group_by_at(my_grouping[-c(1,2)]) |> 
    #   nest() |> 
    #   mutate(
    #     kendall_test = map(
    #       data, 
    #       ~ cor.test(.[[my_target]], .[["campagne_1"]], method = "kendall") |> 
    #         list()),
    #     kendall_pval = map_dbl(
    #       kendall_test, ~ unlist(.) |> pluck(2) |> as.double() |> round(4)
    #     )
    #   )
    
    # Calculate trends only for time-series with more than n data points
    # Timeseries with enough counts
    df_summarised_mincounts <-
      df_summarised |> 
      filter(!is.infinite(get(var_mean))) |> 
      group_by_at(my_grouping[-c(1,2)]) |> 
      nest() |> 
      mutate(census_counts = map_dbl(data, ~nrow(.)))
    
    df_stats_yes <-
      df_summarised_mincounts |>
      filter(census_counts > min_census_counts) |> 
      mutate(
        kendall_test = map(
          data,
          ~ cor.test(.[[var_mean]], .[["campagne_1"]], method = "kendall") |>
            list()),
        
        kendall_pval = map_dbl(kendall_test, ~ unlist(.) |> pluck(2) |> as.double() |> round(3)),
        kendall_tau  = map_dbl(kendall_test, ~ unlist(.) |> pluck(3) |> as.double() |> round(3)
        )
      )
    
    # Timeseries with too little counts
    df_stats_no <-
      df_summarised_mincounts |>
      filter(census_counts <= min_census_counts) |> 
      mutate(
        kendall_test = list(list(NA)),
        kendall_pval = NA,
        kendall_tau  = NA,
      )
    
    df_stats <- bind_rows(df_stats_yes, df_stats_no)
    
    # ______________________________________________________________________________
    # Cleaning
    
    # Get tree count information
    # From df_change and what passed the statistics test, exclude number of trees
    v_year_group <- df_stats |> unnest(data) |>  select(campagne_1, group_1) |> distinct()
    
    tree_count_info <- 
      df_change |>
      select(campagne_1, group_1, data) |> 
      mutate(n = map_dbl(data, ~ nrow(.))) |> 
      select(-data) |> 
      group_by(group_1) |> 
      summarise(across(
        .cols = n, 
        .fns  = sum, na.rm = TRUE,
        .names = "{col}"
      ))
    
    # Create df_plot
    df_pre_plot <- 
      df_summarised |> 
      left_join(tree_count_info, by = join_by(group_1)) |> 
      mutate(
        group_1_f = as.factor(paste0(group_1, " (N = ", n, ")")),
        my_mean = get(var_mean),
        my_se   = get(var_se)
      ) |> 
      select(group_1, group_1_f, campagne_1, my_mean, my_se) 
    
    df_plot <-
      df_pre_plot |> 
      filter(!is.infinite(my_mean))
    
    # Create df_stats
    df_stats <- 
      df_stats |> 
      left_join(tree_count_info, by = join_by(group_1)) |> 
      mutate(group_1_f = as.factor(paste0(group_1, " (N = ", n, ")")))
    
    # Clean factor levels
    # Height class factor needs re-ordering because character 10 comes before 5
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
    
    # ______________________________________________________________________________
    # Create plots
    
    # Show only time-series that have n or more data points
    groups_to_show <- 
      df_summarised_mincounts |>
      filter(census_counts > min_census_counts_for_plot) |> 
      pull(group_1)
    
    if (length(groups_to_show) == 0 ) {
      df_plot <- df_pre_plot |> mutate(my_mean = NA, my_se = NA)
    } else {
      df_plot <- df_plot |> filter(group_1 %in% groups_to_show)
    }
    
    # Set axis limits
    my_ymax <- max(df_plot$my_mean + df_plot$my_se, na.rm = TRUE)
    my_ymin <- min(df_plot$my_mean - df_plot$my_se, na.rm = TRUE)
    
    # Get legend title if available
    my_legend <- name_group_1
    if (name_group_1 == "genus_lat")    { my_legend <- "Species"}
    if (name_group_1 == "height_class") { my_legend <- "Height Class"}
    if (name_group_1 == "age_class")    { my_legend <- "Age Class"}
    
    # ______________________________________________________________________________
    # Facet
    
    plot_facet <- 
      df_plot |> 
      ggplot(
        aes(
          x = campagne_1,
          y = my_mean,
          # color = group_1_f,
          group = group_1_f
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
      geom_text(
        data = df_stats,
        aes(label = paste0("Kendall: p = ", kendall_pval, ", tau = ", kendall_tau),
            x = -Inf,
            y = Inf),
        hjust = -0.25,
        vjust = 1.75,
        fontface = "italic",
      ) +
      ylim(
        my_ymin,
        my_ymax + my_ymax*0.1
      ) +
      theme_classic() +
      labs(
        title   = my_labs$title,
        caption = my_labs$caption,
        x = "Year of First Census",
        y = my_labs$axis
      ) 
    
    # ______________________________________________________________________________
    # All in one
    
    plot_allinone <- 
      df_plot |> 
      ggplot(
        aes(
          x = campagne_1,
          y = my_mean,
          color = group_1_f,
          group = group_1_f
        )
      ) +
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
      # geom_text(
      #   data = df_stats,
      #   aes(label = paste0("Kendall p = ", kendall_pval),
      #       x = 2013,
      #       y = my_ymax*0.975),
      #   vjust = -1,
      #   fontface = "italic",
      # ) +
      scale_color_brewer(palette = "Paired") +
      ylim(
        0,
        my_ymax + my_ymax*0.1
      ) +
      theme_classic() +
      labs(
        color   = my_legend,
        title   = my_labs$title,
        caption = my_labs$caption,
        x = "Year of First Census",
        y = my_labs$axis
      ) 
    
    # ______________________________________________________________________________
    # Output
    
    list_out[[my_target]] <- 
      list(
        df_stats      = df_stats,
        df_plot       = df_plot,
        df_treecount  = tree_count_info,
        plot_facet    = plot_facet,
        plot_allinone = plot_allinone
    )
    
    df_loop$df_stats      <- list(df_stats)
    df_loop$df_plot       <- list(df_plot)
    df_loop$df_treecount  <- list(tree_count_info)
    df_loop$plot_facet    <- list(plot_facet)
    df_loop$plot_allinone <- list(plot_allinone)
    
    df_out <- bind_rows(df_out, df_loop)
    
  }
  
  return(list_out)
  
}
