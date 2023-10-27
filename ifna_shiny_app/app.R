# To deploy, do: rsconnect::deployApp()

# Load Packages ----
# ______________________________________________________________________________

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(sf)
library(here)
library(patchwork)
source("R/f_create_map_from_aggregated_data.R")
source("R/f_load_or_save_latest_file.R")

# Load data ----
# ______________________________________________________________________________
load_or_save_latest_file(nfi_dataset_for_analysis, "load")
my_data <- nfi_dataset_for_analysis

# Set fixed input ----
# Input options

### Spatial Trends ----
my_species_list <- c(
  # "All Species",
  "Quercus",
  "Pinus",
  "Fagus",
  "Carpinus",
  "Castanea",
  "Picea",
  "Abies",
  "Fraxinus",
  "Acer",
  "Betula"
)

all_polys <- c(10, 15, 20, 25)
all_maptypes <- c("dep", "reg", "ser", "gre", "hex")
all_metrics <-
  c("n_mor_yr",
    "n_mor_yr_esq",
    "ba_growth_abs",
    "ba_growth_rate",
    "ba_loss_abs",
    "ba_loss_rate")

paths_to_maps <- 
  readRDS(here("data/tmp/20231017-140959_path_to_maps.rds")) |> 
  str_replace("/Users/pascal/repos/padasch/ifn_analysis", here())

### Temporal Trends ----
# Add new data:
# 1. Add rds file to paths
# 2. Adjust get_filename_for_tt to match filename

tt_groups    <- c("species", "height", "greco")
tt_groups_2g <- c("species_height", "height_species",
                  "species_gre", "gre_species",
                  "height_gre", "gre_height")

tt_paths <- c(
  readRDS(paste0(list.files(recursive = T, pattern = "path_to_plots_tt_gre.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "path_to_plots_tt_species.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "path_to_plots_tt_height.rds")[1]))) |> 
  str_replace("/Users/pascal/repos/padasch/ifn_analysis/ifna_shiny_app", here())

tt_paths_2g <- c(
  readRDS(paste0(list.files(recursive = T, pattern = "tt_species_height.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "tt_height_species.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "tt_species_gre.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "tt_gre_species.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "tt_height_gre.rds")[1])),
  readRDS(paste0(list.files(recursive = T, pattern = "tt_gre_height.rds")[1]))
  ) |> 
  str_replace("/Users/pascal/repos/padasch/ifn_analysis/ifna_shiny_app", here())

### Data Exploration ----
de_my_x_selection <- my_data |> select(where(is.numeric)) |> names()
de_my_y_selection <- my_data |> select(where(is.numeric)) |> names()
de_my_g_selection <- my_data |> select(where(is.factor)) |> names()
de_my_g_selection <- c(de_my_g_selection, "none")

# ______________________________________________________________________________
# Functions ----
get_filename_for_tt <- function(
    my_metric,
    my_group,
    tt_paths,
    facet_or_allinone = NULL
   ) {
  
  # 1 Group
  if (!str_detect(my_group, "_")) {
    # HEIGHT
    if (my_group == "height") {
      my_group <- "tt_height"
    }
    
    # SPECIES
    if (my_group == "species") {
      my_group <- "tt_species"
    }
    
    # GRECO
    if (my_group == "greco") {
      my_group <- "tt_gre"
    }
    
    my_file <- paste0(
      my_group, "-",
      my_metric, "-",
      "plot_", facet_or_allinone,
      ".jpg"
    )
  } else if (str_detect(my_group, "_")) {
    # 2 Groups
    my_file <- paste0(
      "tt_",
      my_group, "-",
      my_metric,
      ".jpg"
    )
    
  } else {
    my_file <- "Filename is wrong!"
    return(my_file)
  }
  
  my_file <- tt_paths[grep(my_file, tt_paths)]
  my_file
}

# Create static dfs ----
# ______________________________________________________________________________
# Those df that need a long time of being created
df_temp_species <- 
  my_data |>
  filter(genus_lat %in% my_species_list) |> 
  group_by(campagne_1, genus_lat) |>
  nest() |>
  mutate(
    n_survived = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "alive") |> nrow()),
    n_died     = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "dead") |> nrow()),
    n_all      = n_survived + n_died,
    mortality  = (1 - (n_survived / n_all) ^ (1 / 5)) * 100
  )

df_temp_height <- 
  my_data |>
  filter(genus_lat %in% my_species_list) |> 
  group_by(height_class, campagne_1, genus_lat) |>
  nest() |>
  drop_na(height_class) |>
  mutate(
    n_survived = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "alive") |> nrow()),
    n_died     = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "dead") |> nrow()),
    n_all      = n_survived + n_died,
    mortality  = (1 - (n_survived / n_all) ^ (1 / 5)) * 100
  )

df_temp_age <- 
  my_data |>
  filter(genus_lat %in% my_species_list) |> 
  group_by(age_class, campagne_1, genus_lat) |>
  nest() |>
  drop_na(age_class) |>
  mutate(
    n_survived = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "alive") |> nrow()),
    n_died     = map_dbl(data, ~filter(., revisit_state == "revisited",
                                       tree_state_1 == "alive",
                                       tree_state_2 == "dead") |> nrow()),
    n_all      = n_survived + n_died,
    mortality  = (1 - (n_survived / n_all) ^ (1 / 5)) * 100
  )

# ______________________________________________________________________________
# UI ----
ui <- fluidPage(
  titlePanel("French NFI Data"),
  
  # Add tags to make embedded images adaptable to screen size
  tags$head(tags$style(
    type="text/css",
    "#hexmap img {max-width: 100%; width: 100%; height: auto}"
  )),
  tags$head(tags$style(
    type="text/css",
    "#temp_species img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  # Temporal Trends
  tags$head(tags$style(
    type="text/css",
    "#tt_allinone img {max-width: 100%; width: 100%; height: auto}"
  )),
  tags$head(tags$style(
    type="text/css",
    "#tt_facet img {max-width: 100%; width: 100%; height: auto}"
  )),
  tags$head(tags$style(
    type="text/css",
    "#tt_2g img {max-width: 100%; width: 100%; height: auto}"
  )),
  
  # Tabs
  tabsetPanel(
    ## Tab: Spatial Trends ----
    tabPanel("Spatial Trends",
             sidebarLayout(
               sidebarPanel(
                 # width = 1.75,
                 selectInput(
                   "species",
                   "Select SPECIES (at the genus level):",
                   choices = unique(my_species_list),
                   selected = "Quercus"
                 ),
                 selectInput(
                   "metric",
                   "Select METRIC of change:",
                   choices = unique(all_metrics),
                   selected = "ba_loss_rate"
                 ),
                 selectInput(
                   "maptype",
                   "Select MAP type:",
                   choices = unique(all_maptypes),
                   selected = "ser"
                 ),
                 selectInput(
                   "polygons",
                   "Select POLYGON size for hexmap",
                   choices = unique(all_polys),
                   selected = 15
                 )
               ),
               mainPanel(
                 h1("Spatial Trends"),
                 plotOutput("hexmap", height = "auto")
               )
             )),
    ## Tab: Temporal Trends (1 Group) ----
    tabPanel(
      "Temporal Trends (1 Group)",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "tt_group",
            "Select grouping for temporal trend",
            choices = tt_groups,
            selected = tt_groups[1]
          ),
          selectInput(
            "tt_metric",
            "Select metric of change",
            choices = all_metrics,
            selected = "ba_loss_rate"
          )
        ),
        mainPanel(
          h1("All Groups"),
          plotOutput("tt_allinone", height = "auto"),
          h1("Split by group"),
          plotOutput("tt_facet", height = "auto")
        )
      )
    ),
    
    ## Tab: Temporal Trends (2 Groups) ----
    tabPanel(
      "Temporal Trends (2 Groups)",
      sidebarLayout(
        sidebarPanel(
          selectInput(
            "tt_group_2g",
            "Select grouping for temporal trend",
            choices = tt_groups_2g,
            selected = tt_groups_2g[1]
          ),
          selectInput(
            "tt_metric_2g",
            "Select metric of change",
            choices = all_metrics,
            selected = "ba_loss_rate"
          )
        ),
        mainPanel(
          plotOutput("tt_2g", height = "auto")
          # textOutput("debug_text")
        )
      )
    ),
    
    ## Tab: Data Exploration ----
    tabPanel("Data Exploration",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "de_my_x",
                   "Select x variable:",
                   choices = de_my_x_selection,
                   selected = "age13"
                 ),
                 selectInput(
                   "de_my_y",
                   "Select y variable:",
                   choices = de_my_y_selection,
                   selected = "htot"
                 ),
                 selectInput(
                   "de_my_g",
                   "Select grouping variable:",
                   choices = de_my_g_selection,
                   selected = "genus_lat"
                 ),
                 numericInput(
                   "de_text_size",
                   "Change text size of figures:",
                   value = 20,
                   step = 0.5
                 )
               ),
               
               mainPanel(
                 h1("Data Exploration"),
                 h2("Scatterplot"),
                 plotOutput("de_scatter", height = "750px"),
                 h2("Linear Regression Models"),
                 tableOutput("de_table"),
                 h2("Distribution of x-variable"),
                 plotOutput("de_distr_x", height = "1000px"),
                 h2("Distribution of y-variable"),
                 plotOutput("de_distr_y", height = "1000px"),
                 h2("Distribution of grouping-variable"),
                 plotOutput("de_distr_g", height = "1000px")
               )
             )),
    
    ## Tab: Datatable ----
    tabPanel("Data", 
             downloadButton("data_dowload", "Download .tsv"),
             dataTableOutput("data_table"))
  )
)


# ______________________________________________________________________________
# Server ----
server <- function(input, output) {
  
  # General ----
  ## Fix plot text size ----
  my_size <- reactive({
    theme(
      text = element_text(size = input$de_text_size),  # Set the base size
      title = element_text(size = input$de_text_size * 1.5),  # Increase title size
      axis.title = element_text(size = input$de_text_size * 1.2),  # Increase axis title size
      axis.text = element_text(size = input$de_text_size * 0.8),  # Decrease axis text size
      legend.text = element_text(size = input$de_text_size * 0.8)  # Decrease legend text size
    )
  })
  
  # Tab: Spatial Trends ----
  # ______________________________________________________________________________
  
  # Get filename
  my_filename <- reactive({
    
    species  <- input$species
    polygons <- input$polygons
    metric   <- input$metric
    maptype  <- input$maptype
    
    if (maptype == "hex") {
      my_file <- paste0(
        species, "-hex_",
        polygons, "-",
        metric,
        ".jpg"
      )
    } else {
      
      my_file <- paste0(
        species, "-",
        maptype, "-",
        metric,
        ".jpg"
      )
    }
    
    my_file <- paths_to_maps[grep(my_file, paths_to_maps)]
    my_file
  })
  
  # Render plots
  output$hexmap <-
    renderImage({
      list(src = paste0(my_filename()))
    }, deleteFile = FALSE)
  
  # Tab: Temporal Trends (1 Group) ----
  # ______________________________________________________________________________
  
  # ALL IN ONE
  tt_filename_allinone <- reactive({
    
    my_group  <- input$tt_group
    my_metric <- input$tt_metric
    
    get_filename_for_tt(
      my_metric,
      my_group,
      tt_paths,
      "allinone"
    )
   
  })
  output$tt_allinone <-
    renderImage({
      list(src = paste0(tt_filename_allinone()))
    }, deleteFile = FALSE)
  
  # FACET 
  tt_filename_facet <- reactive({
    
    my_group  <- input$tt_group
    my_metric <- input$tt_metric
    
    get_filename_for_tt(
      my_metric,
      my_group,
      tt_paths,
      "facet"
    )
  })
  
  output$tt_facet <-
    renderImage({
      list(src = paste0(tt_filename_facet()))
    }, deleteFile = FALSE)
  
  # output$debug_text <-
  #   renderText({
  #     paste(tt_filename_allinone(), " ------------ ",
  #           tt_filename_facet()
  #           )
  #   })
  
  # Tab: Temporal Trends (2 Groups) ----
  # ______________________________________________________________________________
  
  # ALL IN ONE
  tt_filename_2g <- reactive({
    
    my_group  <- input$tt_group_2g
    my_metric <- input$tt_metric_2g
    
    get_filename_for_tt(
      my_metric,
      my_group,
      tt_paths_2g
    )
   
  })
  
  output$tt_2g <-
    renderImage({
      list(src = paste0(tt_filename_2g()))
    }, deleteFile = FALSE)
  
  output$debug_text <-
    renderText({
      paste(tt_filename_2g(), " ------------ ")
    })
  
  # Tab: Data Exploration ----
  # ______________________________________________________________________________
  # df_plot
  
  de_df_plot <- 
    reactive({
      # Get input
      my_x     <- input$de_my_x    
      my_y     <- input$de_my_y    
      my_group <- input$de_my_g
      
      if (my_group == "none") {
        df_plot <- 
          nfi_dataset_for_analysis |> 
          mutate(
            x = get(my_x),
            y = get(my_y)
          ) |> 
          select(x, y) |> 
          drop_na(x, y)
      } else {
        df_plot <- 
          nfi_dataset_for_analysis |> 
          mutate(
            x = get(my_x),
            y = get(my_y),
            g = get(my_group)
          ) |> 
          select(x, y, g) |> 
          drop_na(x, y, g)
        
        top_10 <- 
          df_plot$g |> 
          table() |> 
          sort(TRUE) |> 
          head(12) |> 
          as.data.frame() |> 
          rename(g = Var1, n = Freq) |> 
          arrange(g)
        
        df_plot <- 
          df_plot |> 
          filter(g %in% top_10$g) |> 
          left_join(top_10, by = join_by(g)) |> 
          mutate(g = as.factor(paste0(g, " (N = ", n,")"))) |> 
          arrange(g)
      }
      
      df_plot
    })
  
  ## Scatter plot ----
  output$de_scatter <- renderPlot({
    # Get input
    my_x     <- input$de_my_x    
    my_y     <- input$de_my_y    
    my_group <- input$de_my_g
    df_plot  <- de_df_plot()
    
    if (my_group == "none") {
      p <- 
        df_plot |> 
        ggplot() +
        aes(x = x, y = y) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "lm") +
        labs(
          x = my_x,
          y = my_y
        ) +
        theme_classic()
    } else {
      p <- 
        df_plot |> 
        ggplot() +
        aes(x = x, y = y, group = g) +
        facet_wrap(~g) +
        geom_point(alpha = 0.2) +
        geom_smooth(method = "lm") +
        labs(
          x = my_x,
          y = my_y,
          color = my_group,
          fill = my_group
        ) +
        theme_classic()
    }
    p +
      theme(
        legend.position = "bottom",
        text = element_text(size = input$de_text_size),  # Set the base size
        title = element_text(size = input$de_text_size * 1.5),  # Increase title size
        axis.title = element_text(size = input$de_text_size * 1.2),  # Increase axis title size
        axis.text = element_text(size = input$de_text_size * 0.8),  # Decrease axis text size
        legend.text = element_text(size = input$de_text_size * 0.8)  # Decrease legend text size
      )
  })
  
  ## LM Table ----
  output$de_table <- renderTable({
    # Get input
    my_x     <- input$de_my_x    
    my_y     <- input$de_my_y    
    my_group <- input$de_my_g
    df_plot  <- de_df_plot()

    if (my_group == "none") {    
      my_lm <- lm(y ~ x, data = df_plot) |> summary()
      de_table <- 
        tibble(
        intercept_est   = my_lm$coefficients[1, 1],
        slope_est       = my_lm$coefficients[2, 1],
        slope_se        = my_lm$coefficients[2, 2],
        slope_pval      = my_lm$coefficients[2, 4],
        R2              = my_lm$r.squared,
        adjR2           = my_lm$adj.r.squared)
      
    } else {
      de_table <- 
        df_plot |>
        group_by(g) |>
        nest() |>
        mutate(
          lm_result       = map(data, ~lm(y ~ x, data = .)),
          model_summary   = map(lm_result, ~summary(.)),
          intercept_est   = map_dbl(model_summary, ~.$coefficients[1, 1]),
          intercept_se    = map_dbl(model_summary, ~.$coefficients[1, 2]),
          intercept_pval  = map_dbl(model_summary, ~.$coefficients[1, 4]),
          slope_est       = map_dbl(model_summary, ~.$coefficients[2, 1]),
          slope_se        = map_dbl(model_summary, ~.$coefficients[2, 2]),
          slope_pval      = map_dbl(model_summary, ~.$coefficients[2, 4]),
          R2              = map_dbl(model_summary, ~.$r.squared),
          adjR2           = map_dbl(model_summary, ~.$adj.r.squared)
        ) |> 
        select(-data, -lm_result, -model_summary, -R2)
    }
    de_table
    
  })
  
  ## Distr ----
  output$de_distr_x <- renderPlot({
    # Get input
    my_x     <- input$de_my_x    
    my_y     <- input$de_my_y    
    my_group <- input$de_my_g
    df_plot  <- de_df_plot()
    
    if (my_group == "none") {
      p <- 
        df_plot |> 
        ggplot() +
        aes(x = x) +
        geom_density() +
        geom_boxplot(
          width = 0.001,
          color = "grey50",
          alpha = 0.5,
          outlier.color = "red"
        ) +
        scale_fill_viridis_d() +
        labs(x = my_x) +
        theme_classic()
      
    } else {
      p <- 
        df_plot |> 
        ggplot() +
        aes(x = x, y = g, fill = factor(stat(quantile))) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          scale = 0.85,
          calc_ecdf = TRUE,
          quantiles = 4, 
          quantile_lines = TRUE, 
          jittered_points = TRUE,
          position = position_points_jitter(width = 0.05, height = 0, yoffset = -0.1),
          point_shape = '|', 
          point_size = 6, 
          point_alpha = 0.75, 
          alpha = 0.7
        ) +
        scale_fill_viridis_d(name = "Quartiles") +
        labs(
          x = my_x,
          y = my_group
          ) +
        theme_classic()
    }
    
      p +
        theme(
          legend.position = "bottom",
          text = element_text(size = input$de_text_size),  # Set the base size
          title = element_text(size = input$de_text_size * 1.5),  # Increase title size
          axis.title = element_text(size = input$de_text_size * 1.2),  # Increase axis title size
          axis.text = element_text(size = input$de_text_size * 0.8),  # Decrease axis text size
          legend.text = element_text(size = input$de_text_size * 0.8)  # Decrease legend text size
        )
  })
 
output$de_distr_y <- renderPlot({
    # Get input
    my_x     <- input$de_my_x    
    my_y     <- input$de_my_y    
    my_group <- input$de_my_g
    df_plot  <- de_df_plot()
    
    if (my_group == "none") {
     p <- 
        df_plot |> 
        ggplot() +
        aes(x = y) +
        geom_density() +
        geom_boxplot(
          width = 0.001,
          color = "grey50",
          alpha = 0.5,
          outlier.color = "red"
        ) +
        scale_fill_viridis_d() +
        labs(x = my_y) +
        theme_classic()
      
    } else {
      p <- 
        df_plot |> 
        ggplot() +
        aes(x = y, y = g, fill = factor(stat(quantile))) +
        stat_density_ridges(
          geom = "density_ridges_gradient",
          scale = 0.85,
          calc_ecdf = TRUE,
          quantiles = 4, 
          quantile_lines = TRUE, 
          jittered_points = TRUE,
          position = position_points_jitter(width = 0.05, height = 0, yoffset = -0.1),
          point_shape = '|', 
          point_size = 6, 
          point_alpha = 0.75, 
          alpha = 0.7
        ) +
        scale_fill_viridis_d(name = "Quartiles") +
        labs(
          x = my_y,
          y = my_group
          ) +
        theme_classic()
    }
    
      p +
        theme(
          legend.position = "bottom",
          text = element_text(size = input$de_text_size),  # Set the base size
          title = element_text(size = input$de_text_size * 1.5),  # Increase title size
          axis.title = element_text(size = input$de_text_size * 1.2),  # Increase axis title size
          axis.text = element_text(size = input$de_text_size * 0.8),  # Decrease axis text size
          legend.text = element_text(size = input$de_text_size * 0.8)  # Decrease legend text size
        )
  })
  
output$de_distr_g <- renderPlot({
    # Get input
    my_x     <- input$de_my_x    
    my_y     <- input$de_my_y    
    my_group <- input$de_my_g
    df_plot  <- de_df_plot()
    
    if (my_group == "none") {
     p <- ggplot()
      
    } else {
      p <- 
        df_plot |> 
        ggplot() +
        aes(y = g) +
        labs(
          x = "Count",
          y = my_group
        ) +
        geom_bar() +
        theme_classic()
    }
    
      p +
        theme(
          legend.position = "bottom",
          text = element_text(size = input$de_text_size),  # Set the base size
          title = element_text(size = input$de_text_size * 1.5),  # Increase title size
          axis.title = element_text(size = input$de_text_size * 1.2),  # Increase axis title size
          axis.text = element_text(size = input$de_text_size * 0.8),  # Decrease axis text size
          legend.text = element_text(size = input$de_text_size * 0.8)  # Decrease legend text size
        )
  })
  
  # Tab: Datatable ----
  # ______________________________________________________________________________
  output$data_table <- renderDataTable({
    my_data
  })
}

# Run the application
shinyApp(ui = ui, server = server)
