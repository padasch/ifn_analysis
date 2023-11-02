source("R/_setup.R")
load_or_save_latest_file(nfi_dataset_for_analysis, "load")

var_num <- 
  nfi_dataset_for_analysis |> 
  select(where(is.numeric)) |> 
  names()

var_fac <- 
  nfi_dataset_for_analysis |> 
  select(where(is.factor)) |> 
  names()

var_chr <- 
  nfi_dataset_for_analysis |> 
  select(where(is.character)) |> 
  names()

var_log <-
  nfi_dataset_for_analysis |> 
  select(where(is.logical)) |> 
  names()

length(var_num) + length(var_fac) + length(var_chr) + length(var_log)
ncol(nfi_dataset_for_analysis)


my_x     <- sample(var_num, 1)
my_y     <- sample(var_num, 1)
my_group <- sample(var_fac, 1)

while (my_x == my_y) {my_y <- sample(var_num, 1)}

my_x <- "age13"
my_y <- "htot"
my_group <- "genus_lat"


# WITHOUT GROUPING VARIABLE
# if (my_group == "none") {
#   
# }
df_plot <- 
  nfi_dataset_for_analysis |> 
  mutate(
    x = get(my_x),
    y = get(my_y)
  ) |> 
  select(x, y)

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

# Linear Model
my_lm <- lm(y ~ x, data = df_plot) |> summary()

tibble(
  intercept_est   = my_lm$coefficients[1, 1],
  intercept_se    = my_lm$coefficients[1, 2],
  intercept_pval  = my_lm$coefficients[1, 4],
  slope_est       = my_lm$coefficients[2, 1],
  slope_se        = my_lm$coefficients[2, 2],
  slope_pval      = my_lm$coefficients[2, 4],
  R2              = my_lm$r.squared,
  adjR2           = my_lm$adj.r.squared
  ) |> 
  knitr::kable(digits = 2)

# Densities
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


# ______________________________________________________________________________
# WITH GROUPING VARIABLE
top_10 <- 
  nfi_dataset_for_analysis[[my_group]] |> 
  table() |> 
  sort(TRUE) |> 
  head(12) |> 
  as.data.frame() |> 
  rename(g = Var1, n = Freq)

df_plot <- 
  nfi_dataset_for_analysis |> 
  mutate(
    x = get(my_x),
    y = get(my_y),
    g = get(my_group)
  ) |> 
  select(x, y, g) |> 
  filter(g %in% top_10$g) |> 
  left_join(top_10, by = join_by(g)) |> 
  mutate(g = paste0(g, " (N = ", n," )"))

# Linear Models
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

df_plot %>%
  group_by(g) %>%
  nest() %>%
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
  select(-data, -lm_result, -model_summary, -R2) |>
  knitr::kable(digits = 2)
  
# Densities
df_plot |> 
  ggplot() +
  aes(x = x, y = g, fill = g) +
  geom_violin(width = 0.8) +
  geom_boxplot(
    width = 0.1,
    color = "grey50",
    alpha = 0.5,
    outlier.color = "red"
  ) +
  scale_fill_viridis_d() +
   labs(
    x = my_x,
    y = my_group,
    color = my_group,
    fill = my_group
  ) +
  theme_classic()

df_plot |> 
  ggplot() +
  aes(x = y, y = g, fill = factor(stat(quantile))) +
  stat_density_ridges(
    geom = "density_ridges_gradient",
    scale = 0.9,
    calc_ecdf = TRUE,
    quantiles = 4, 
    quantile_lines = TRUE
    ) +
  scale_fill_viridis_d(name = "Quartiles") +
  labs(
    x = my_y,
    y = my_group,
    color = my_group,
    fill = my_group
  ) +
  theme_classic()



