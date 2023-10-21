# To deploy, do: rsconnect::deployApp()

# Load Packages ----
# ______________________________________________________________________________

library(shiny)
library(tidyverse)
library(ggplot2)
library(ggridges)
library(sf)
library(here)
source("R/f_create_hexmap_from_aggregated_data.R")
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
de_my_g_selection <- c(de_my_g_selection, "g")

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
                   "species_de",
                   "Select SPECIES (at the genus level):",
                   choices = unique(my_species_list),
                   selected = unique(my_species_list)[1]
                 ),
                 selectInput(
                   "tree_metric",
                   "Select TREE Metric",
                   choices = c("Circumference", "Diameter BH", "Basal Area"),
                   selected = "Basal Area"
                 ),
                 selectInput(
                   "change_metric",
                   "Select CHANGE Metric",
                   choices = c("Absolute", "Relative"),
                   selected = "Relative"
                 ),
                 selectInput(
                   "grouping_variable",
                   "Select GROUPING Variable",
                   choices = c("by Age", "by Height"),
                   selected = "by Height"
                 ),
                 numericInput("xlim_max",
                              "Enter UPPER limit of x-axis:",
                              value = 20),
                 numericInput("xlim_min",
                              "Enter LOWER limit of y-axis:",
                              value = -5)
                 ,
                 numericInput(
                   "text_size",
                   "Change plot TEXT size:",
                   value = 10
                 )
               ),
               
               mainPanel(
                 h1("Data Exploration"),
                 markdown("Vertical lines show mean of distribution"),
                 plotOutput("dataexp1", height = 1000)
               )
             )),
    
    ## Tab: Datatable ----
    tabPanel("Data", dataTableOutput("data_table")),
  )
)


# ______________________________________________________________________________
# Server ----
server <- function(input, output) {
  
  # General ----
  ## Fix plot text size ----
  my_size <- reactive({
    theme(
      text = element_text(size = input$text_size),  # Set the base size
      title = element_text(size = input$text_size * 1.5),  # Increase title size
      axis.title = element_text(size = input$text_size * 1.2),  # Increase axis title size
      axis.text = element_text(size = input$text_size * 0.8),  # Decrease axis text size
      legend.text = element_text(size = input$text_size * 0.8)  # Decrease legend text size
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
  
  my_list <- 
    reactive({
      
      set_list <- list()
      
      # Metric to plot
      if (input$tree_metric == "Circumference" & input$change_metric == "Absolute") {
        set_list$var <- "c13_change_abs_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [m/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      } else if (input$tree_metric == "Circumference" & input$change_metric == "Relative") {
        set_list$var <- "c13_change_perc_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [%/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      } else if (input$tree_metric == "Diameter BH" & input$change_metric == "Absolute") {
        set_list$var <- "dbh_change_abs_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [m/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      } else if (input$tree_metric == "Diameter BH" & input$change_metric == "Relative") {
        set_list$var <- "dbh_change_perc_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [%/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      } else if (input$tree_metric == "Basal Area" & input$change_metric == "Absolute") {
        set_list$var <- "ba_change_abs_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [m^2/ha/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      } else if (input$tree_metric == "Basal Area" & input$change_metric == "Relative") {
        set_list$var <- "ba_change_perc_yr"
        set_list$title  <- paste0(input$change_metric, " Change of ", 
                                  input$tree_metric, " in [%/yr]")
        set_list$subtitle <- paste0(
          "For ", input$species_de, " grouped ", input$grouping_variable
        )
      }
      
      set_list
    })
  
  # Growth by age class
  output$dataexp1 <- renderPlot({
    
    set_list <- my_list()
    
    # Filter data if needed
    if (input$species_de == "All Species") {
      my_df <- my_data
    } else {
      my_df <- my_data |> filter(genus_lat == input$species_de)
    }
    
    if (input$grouping_variable == "by Age") {
      my_group <- "age_class"
    } else {
      my_group <- "height_class"
    }
    
    # Create plot
    my_df |>
      mutate(
        var = get(set_list$var),
        grouping = get(my_group),
        campagne_1 = fct_rev(as.factor(campagne_1))) |> 
      drop_na(
        var, 
        grouping
        ) |> 
      ggplot() +
      aes(
        x = var,
        y = campagne_1,
        fill = campagne_1, 
      ) +
      stat_density_ridges(quantile_lines = TRUE,
                          quantiles = 0.5,
                          jittered_points = TRUE,
                          alpha = 0.7) +
      xlim(input$xlim_min, 
           input$xlim_max) +
      scale_fill_viridis_d() +
      facet_wrap(~grouping, nrow = 3) +
      labs(
        title = set_list$title,
        subtitle = set_list$subtitle,
        y     = "Year of First Census",
        fill  = "Year of First Census",
        x     = "Change in chosen metric"
      ) +
      guides(fill = guide_legend(reverse = TRUE, nrow = 1)) +
      theme(
        legend.position = "bottom",
        text = element_text(size = input$text_size),  # Set the base size
        title = element_text(size = input$text_size * 1.5),  # Increase title size
        axis.title = element_text(size = input$text_size * 1.2),  # Increase axis title size
        axis.text = element_text(size = input$text_size * 0.8),  # Decrease axis text size
        legend.text = element_text(size = input$text_size * 0.8)  # Decrease legend text size
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
