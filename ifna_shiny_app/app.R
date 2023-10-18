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

# Load data ----
# ______________________________________________________________________________
my_data <- readRDS("data/tmp/final_dataset_for_analysis.rds")

# Set fixed input ----
# Input options
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

all_files <- 
  readRDS(here("data/tmp/20231017-140959_path_to_maps.rds")) |> 
  str_replace("/Users/pascal/repos/padasch/ifn_analysis", here())

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
    ## Tab: Temporal Trends ----
    tabPanel(
      "Temporal Trends",
      sidebarLayout(
        sidebarPanel(
          markdown("No options available.")
        ),
        mainPanel(
          h1("Temporal Trends"),
          plotOutput("temp_species"),
          h1(""),
          plotOutput("temp_height", height = 500),
          h1(""),
          plotOutput("temp_age", height = 500)
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
    
    my_file <- all_files[grep(my_file, all_files)]
    my_file
  })
  
  # Render plots
  output$hexmap <-
    renderImage({
      list(src = paste0(my_filename()))
    }, deleteFile = FALSE)
  
  # Tab: Temporal Trends ----
  # ______________________________________________________________________________
  output$temp_species <- 
    renderPlot({
      df_temp_species |> 
        ggplot() +
        aes(
          x = campagne_1,
          y = mortality,
          color = genus_lat
        ) +
        geom_line() +
        geom_point() +
        scale_color_brewer(palette = "Paired") +
        ylim(0, 4) +
        labs(
          title = "Mortality Trend by Species",
          y = "Stem-based Mortality [%/yr]",
          x = "Year of first census",
          color = "Species") +
        my_size()
    })
  
  output$temp_height <- 
    renderPlot({
      df_temp_height |> 
        ggplot() +
        aes(
          x = campagne_1,
          y = mortality,
          color = height_class
        ) +
        geom_line() +
        geom_point() +
        facet_wrap(~genus_lat, nrow = 2) +
        scale_color_viridis_d(end = 0.7) +
        ylim(0, 10) +
        labs(
          title = "Mortality Trend by Species and Height Class",
          y = "Stem-based Mortality [%/yr]",
          x = "Year of first census",
          color = "Height Class") +
        my_size()
    })
  
  output$temp_age <- 
    renderPlot({
      df_temp_age |> 
        ggplot() +
        aes(
          x = campagne_1,
          y = mortality,
          color = age_class
        ) +
        geom_line() +
        geom_point() +
        facet_wrap(~genus_lat, nrow = 2) +
        scale_color_viridis_d(end = 0.7) +
        ylim(0, 4) +
        labs(
          title = "Mortality Trend by Species and Age Class",
          y = "Stem-based Mortality [%/yr]",
          x = "Year of first census",
          color = "Age Class") +
        my_size()
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
