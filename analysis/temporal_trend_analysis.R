# Temporal Trend Analysis ----
# Setup ----
source(here::here("R/_setup.R"))
load_or_save_latest_file(nfi_dataset_for_analysis, "load")

# Location-based Analysis ----
# └ Inputs ----
# Manual
group_by_species_or_height <- "height"
min_trees_per_site <- 3 # Minimum number of trees per site (increases computation time)
n_spec             <- 9 # Number of species to be analysed
my_target          <- "ba_loss_rate" # Plotting target
df_in              <- df_in

# Automated
# Define base grouping
# Important: Keep idp at first position here!
my_grouping        <- c("idp", "campagne_1", "grouping_var")

# Define species grouping
if (group_by_species_or_heright == "species") {
  most_common_spec <- 
    get_most_common_species(df_in, "genus") |> 
    slice(1:n_spec)
  
  df_tmp <- 
    df_in |> 
    filter(genus_lat %in% most_common_spec$genus_lat) |> 
    rename(grouping_var = genus_lat)
  
  # Define height grouping
} else if (group_by_species_or_height == "height")  {
  df_tmp <- df_in |> rename(grouping_var = height_class)
} else {
  stop()
}

## Variables used later
var_mean <- paste0(my_target, "_mean")
var_se   <- paste0(my_target, "_se")

## ggplot
txt_caption <- paste0("Data was filtered for sites with at least ", min_trees_per_site, " of the given species.")
txt_title   <- paste0("Trends of Natural Mortality TODO")



# └ All Species ----

# └─ Change per site ----
# - Data needs to be nested by plot/year/species or height or both

# Drop entries without height class

df_nested <-
  df_tmp |> 
  drop_na(grouping_var) |>   # Remove where grouping information is NA
  select(                    # Select only relevant variables to speed up computation
    idp,
    campagne_1,
    grouping_var,
    # genus_lat,
    # height_class,
    # age_class,
    tree_state_change,
    ba_2,
    ba_1
  ) |>
  # filter(genus_lat == my_species) |> 
  group_by_at(my_grouping) |> 
  filter(n() >= min_trees_per_site) |> 
  nest()

tic()
df_change <- 
  df_nested |> 
  calculate_growth_mortality() |> 
  ungroup()
toc()
beep()

# └─ Change per grouping ----
vars_to_summarise <- df_change |> select(where(is.numeric), -campagne_1) |> names()

df_summarised <- 
  df_change |> 
  group_by_at(my_grouping[-1]) |> 
  mutate(count = n()) |> 
  summarise(across(
    .cols = all_of(vars_to_summarise), 
    .fns  = list(mean = mean, se = std_error), na.rm = TRUE, 
    .names = "{col}_{fn}"
  ))

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

# Trend over means 
df_stats <-
  df_summarised |>
  group_by_at(my_grouping[-c(1,2)]) |>
  nest() |>
  mutate(
    kendall_test = map(
      data,
      ~ cor.test(.[[var_mean]], .[["campagne_1"]], method = "kendall") |>
        list()),
    kendall_pval = map_dbl(
      kendall_test, ~ unlist(.) |> pluck(2) |> as.double() |> round(4)
    )
  )

# df_stats

# └ Plot it ----

# Add tree count information
tree_count_info <- 
  df_change |>
  select(grouping_var, data) |>
  unnest(data) |>
  count(grouping_var)

df_plot <- 
  df_summarised |> 
  left_join(tree_count_info, by = join_by(grouping_var)) |> 
  mutate(grouping_var = as.factor(paste0(grouping_var, " (N = ", n, ")")))

df_stats <- 
  df_stats |> 
  left_join(tree_count_info, by = join_by(grouping_var)) |> 
  mutate(grouping_var = as.factor(paste0(grouping_var, " (N = ", n, ")")))

# Height class factor needs re-ordering because character 10 comes before 5
if (group_by_species_or_height == "height") {
  df_plot$grouping_var = factor(df_plot$grouping_var, levels = levels(df_stats$grouping_var))
}


my_ymax <- max(df_plot[[var_mean]] + df_plot[[var_se]])
my_ymin <- min(df_plot[[var_mean]] - df_plot[[var_se]])

df_plot |> 
  ggplot(
    aes(
      x = campagne_1,
      y = get(var_mean),
      # color = grouping_var,
      group = grouping_var
    )
  ) +
  facet_wrap(~grouping_var) +
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
  geom_text(
    data = df_stats,
    aes(label = paste0("Kendall p = ", kendall_pval),
        x = 2013,
        y = my_ymax*0.975),
    vjust = -1,
    fontface = "italic",
  ) +
  ylim(
    my_ymin,
    my_ymax + my_ymax*0.1
    ) +
  theme_classic() +
  labs(
    title   = txt_title,
    caption = txt_caption,
    x = "Year of First Census",
    y = my_target
  ) 


# └ Region-based Analysis ----
# TODO: The idea of this section is to include a bootstrapping approach to calculate
# the uncertainty of the data. This comes at the advantage that we are taking together all data
# that we have on the trees and not subset for plots where there have been only a few trees 
# of a given species.

