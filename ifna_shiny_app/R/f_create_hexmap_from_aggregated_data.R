create_map_from_aggregated_data <- function(
    selected_var = NULL,
    df_plot  = NULL,
    return_or_save = "return",
    selected_species = NULL
    ) {
  
  # Input Check
  # ______________________________________________________________________________
  # selected_var:
  # - Variable to be plotted, must be of the one's returned from empty function call
  if (is.null(selected_var)) {
    stop("Variables that can be selected: \n" ,
         " [n_mor_yr] returns %-stem based mortality per year following Hoshino et al. 2002\n",
         " [n_mor_yr_esq] returns %-stem based mortality per year following Esquivel et al. 2020\n",
         " [ba_growth_abs] returns absolute BA growth in m^2 / hectare / yr \n",
         " [ba_growth_rate] returns relative BA growth in % \n",
         " [ba_loss_abs] returns absolute BA loss in m^2 / hectare / yr \n",
         " [ba_loss_rate] returns relative BA loss in % \n"
         )
  } else if (!is.character(selected_var)) {stop("selected_var input is wrong")}
  
  # df_plot:
  # - tibble with hexagon (or other) geometry
  if (!all(sf::st_is_valid(df_plot))) stop("df_plot input is wrong")
  
  # return_or_save
  # - Character indicating whether to "save", "return", or do "both"
  if (!(return_or_save %in% c("save", "return", "both"))) stop("return_or_save input is wrong")
  
  # selected_species
  # - String, indicating species that was filtered for
  
  # DEBUG SECTION ----
  # ______________________________________________________________________________
  # selected_var <- "n_mor_yr"
  # selected_var <- "n_mor_yr_esq"
  # selected_var <- "ba_growth_abs"
  # selected_var <- "ba_growth_rate"
  # selected_var <- "ba_loss_abs"
  # selected_var <- "ba_loss_rate"
  # Rough plot for testing scales
  # df_plot |> drop_na(NAME_1) |> 
  #   ggplot(aes(fill = get(selected_var), color = get(selected_var))) +
  #   geom_sf(lwd = 0.1) +
  #   facet_wrap(~census_interval)
  
  # ______________________________________________________________________________
  # Set plot info per input variable ----
  # - Make a boxplot of the variable to get a feeling for useful breaks!
  
  ## MORTALITY - %-stem based ----
  if (selected_var == "n_mor_yr_esq" | selected_var == "n_mor_yr") {
    legend_txt <- expression(paste("\nTree Mortality [%-stems ",yr ^ -1, "]"))
    breaks     <- c(-Inf, 0.5, 1, 1.5, 2, 2.5, 3, Inf)
    breaks_txt <- c(paste0("<", breaks[2:(length(breaks) - 1)], "%"),
                    paste0("≥", breaks[(length(breaks) - 1)], "%"))
    palette_txt <- "Reds"
    caption_txt <- ifelse(
      selected_var == "n_mor_yr_esq",
      "\nMortality based on Esquivel et al. 2020:  m = (1 − (n_sur / n_ini)^(1/T)) × 100",
      "\nMortality based on Hoshino et al. 2002:  m = log(n_ini / n_sur) / 5 * 100"
    )
    my_title <- paste0("Mortality of ", selected_species, "\n")
  }
  
  ## MORTALITY - Basal Area ----
  if (selected_var == "ba_loss_abs") {
    legend_txt <- expression(paste("\nTree Mortality [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    breaks     <- c(-Inf, 1, 2, 3, 4, 5, Inf)
    breaks_txt <- c(paste0("<", breaks[2:(length(breaks) - 1)], "%"),
                    paste0("≥", breaks[(length(breaks) - 1)], "%"))
    palette_txt <- "Reds"
    caption_txt <- "\nMortality = [BA of trees that died between census] / 5"
    my_title <- paste0("Mortality of ", selected_species, "\n")
  }
  
  if (selected_var == "ba_loss_rate") {
    legend_txt <- expression(paste("\nTree Mortality Rate [%-basal area ",yr ^ -1, "]"))
    breaks     <- c(-Inf, 0.25, 0.5, 1.75, 1, 1.25, Inf)
    breaks_txt <- c(paste0("<", breaks[2:(length(breaks) - 1)], "%"),
                    paste0("≥", breaks[(length(breaks) - 1)], "%"))
    palette_txt <- "Reds"
    caption_txt <- "\nMortality = [BA of trees that died between census] / [BA of all alive trees at 1st visit] / 5"
    my_title <- paste0("Mortality of ", selected_species, "\n")
  }
  
  ## GROWTH - Basal Area ----
  if (selected_var == "ba_growth_abs") {
    legend_txt <- expression(paste("\nTree Growth [", m ^ 2 ~ hectare ^ -1 ~ yr ^ -1, "]"))
    breaks     <- c(-Inf, 5, 10, 15, 20, 25, Inf)
    breaks_txt <- c(paste0("<", breaks[2:(length(breaks) - 1)], "%"),
                    paste0("≥", breaks[(length(breaks) - 1)], "%"))
    palette_txt <- "Greens"
    caption_txt <- "\nGrowth = ([BA of survivors at 2nd visit] - [BA of survivors at 1st visit]) / 5"
    my_title <- paste0("Growth of ", selected_species, "\n")
  }
  
  if (selected_var == "ba_growth_rate") {
    legend_txt <- expression(paste("\nTree Growth Rate [%-basal area ",yr ^ -1, "]"))
    breaks     <- c(-Inf, 0.5, 1, 1.5, 2, 2.5, 3, Inf)
    breaks_txt <- c(paste0("<", breaks[2:(length(breaks) - 1)], "%"),
                    paste0("≥", breaks[(length(breaks) - 1)], "%"))
    palette_txt <- "Greens"
    caption_txt <- "\nGrowth = ([BA of survivors at 2nd visit] - [BA of survivors at 1st visit]) / ([BA of survivors at 1st visit]) 5"
    my_title <- paste0("Growth of ", selected_species, "\n")
  }
  
  # Turn variable into classes ----
  df_plot <-
    df_plot |> 
    mutate(clss = 
             cut(get(selected_var), 
                 breaks = breaks, 
                 labels = breaks_txt, 
                 include.lowest = TRUE)
    )
  
  # Set custom theme ----
  my_theme <-
    theme_void() +
    theme(
      plot.margin = margin(1, 1, 10, 1, "pt"),
      plot.background = element_rect(fill = "white", color = NA),
      plot.title = 
        element_text(
          hjust = 0.5,
          face = "bold"
        ),
      legend.position = "bottom",
      legend.title = element_text(
        hjust = 0.5,
        color = "black",
        face = "bold"
      ),
      legend.text = element_text(color = "black"),
      plot.caption = element_text(hjust = 0.5),
      strip.text = element_text(size = 10, face = "bold") # facet wrap titles
      
    )
  
  # Make plot ----
  p <- 
    df_plot |> drop_na(NAME_1) |> 
    ggplot(aes(fill = clss, color = clss)) +
    geom_sf(lwd = 0.1) +
    facet_wrap(~census_interval, nrow = 2) +
    scale_fill_brewer(
      palette   = palette_txt,
      na.value = "grey80",
      name = legend_txt
    ) +
    scale_color_brewer(
      palette   = palette_txt,
      na.value = "grey80",
      name = legend_txt
    ) +
    # scale_fill_viridis_d(
    #   option   = "A",
    #   na.value = "grey90",
    #   name = legend_txt
    #   ) +
    # scale_color_viridis_d(
    #   option   = "A",
    #   na.value = "grey90",
    #   name = legend_txt
    #   ) +
    guides(fill = guide_legend(nrow = 2, title.position = "top")) +
    labs(title = my_title,
         caption = caption_txt) +
    my_theme
  
  # Return or save plot ----
  if (return_or_save == "save" | return_or_save == "both") {
    
    # Set filename
    my_dir <- get_todays_file_directory("figures")
    my_filename <-
      paste0(my_dir,
             "/",
             format(Sys.time(), format = "%H%M%S"),
             "_",
             selected_var,
             ".pdf")
    # Save it
    ggsave(my_filename,
           p,
           device = cairo_pdf,
           height = 7,
           width  = 11)
  }
  
  if (return_or_save == "return" | return_or_save == "both") {
    return(p)
  }
}
