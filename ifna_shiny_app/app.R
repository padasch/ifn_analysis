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
  "All Species",
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

my_polygons_list <- seq(15, 60, 15)

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
                   selected = unique(my_species_list)[1]
                 ),
                 selectInput(
                   "polygons",
                   "Select Number of POLYGONS per º of lat/lon:",
                   choices = unique(my_polygons_list),
                   selected = unique(my_polygons_list)[1]
                 ),
                 numericInput("text_size",
                              "Change TEXT size:",
                              value = 14)
               ),
               mainPanel(
                 h1("Spatial Trends"),
                 h2("Mortality"),
                 h4("%-stem based mortality (Hoshino et al. 2002)"),
                 plotOutput("hexmap1", height = 750),
                 h4("%-stem based mortality (Esquivel et al. 2020)"),
                 plotOutput("hexmap2", height = 750),
                 h4("Absolute loss of basal area"),
                 plotOutput("hexmap3", height = 750),
                 h4("Relative loss of basal area"),
                 plotOutput("hexmap4", height = 750),
                 h2("Growth"),
                 h4("Absolute Growth"),
                 plotOutput("hexmap5", height = 750),
                 h4("Relative Growth"),
                 plotOutput("hexmap6", height = 750)
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
          plotOutput("temp_species", height = 500),
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
                 # ,
                 # numericInput(
                 #   "text_size",
                 #   "Change plot TEXT size:",
                 #   value = 10
                 # )
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
    tmp_species  <- input$species
    tmp_polygons <- input$polygons
    
    if (input$species == "All Species") {
      my_filename <-
        paste0(
          here("data/tmp/"),
          "hexmap_data-",
          "all_species", "-",
          tmp_polygons, "polygons_per_degree-",
          5, "min_ntrees_per_polygon",
          ".rds"
        )
      
    } else {
      my_filename <-
        paste0(
          here("data/tmp/"),
          "hexmap_data-",
          tmp_species, "-",
          tmp_polygons, "polygons_per_degree-",
          5, "min_ntrees_per_polygon",
          ".rds"
        )
    }
    
    
    my_filename
  })
  
  # Get data 
  my_df <- reactive({
    readRDS(my_filename())
  })
  
  # Render plots
  output$hexmap1 <-
    renderPlot({
      create_hexmap_from_aggregated_data("n_mor_yr", my_df(), "return", input$species) + my_size()
    })
  output$hexmap2 <-
    renderPlot({
      create_hexmap_from_aggregated_data("n_mor_yr_esq", my_df(), "return", input$species) + my_size()
    })
  output$hexmap3 <-
    renderPlot({
      create_hexmap_from_aggregated_data("ba_loss_abs", my_df(), "return", input$species) + my_size()
    })
  output$hexmap4 <-
    renderPlot({
      create_hexmap_from_aggregated_data("ba_loss_rate", my_df(), "return", input$species) + my_size()
    })
  output$hexmap5 <-
    renderPlot({
      create_hexmap_from_aggregated_data("ba_growth_abs", my_df(), "return", input$species) + my_size()
    })
  output$hexmap6 <-
    renderPlot({
      create_hexmap_from_aggregated_data("ba_growth_rate", my_df(), "return", input$species) + my_size()
    })
  
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
