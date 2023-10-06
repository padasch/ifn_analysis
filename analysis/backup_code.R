# 2023-10-06 ----
## Trajectory Code

```{r}
df_tmp2 <- 
  df_tmp |> 
  ungroup() |> 
  mutate(
    region = map_chr(data, ~pull(., region_name) |> unique()),
    region = as.factor(region)) |> 
  select(-data) |> 
  group_by(campagne_1, region) |> 
  nest() |> 
  mutate(
    mean_ba_loss_yr = map_dbl(data, ~pull(., ba_loss_yr) |> mean(na.rm = TRUE)),
    mean_ba_gain_yr = map_dbl(data, ~pull(., ba_gain_yr) |> mean(na.rm = TRUE)),
    mean_n_mor_yr = map_dbl(data, ~pull(., n_mor_yr) |> mean(na.rm = TRUE)),
    mean_n_rec_yr = map_dbl(data, ~pull(., n_rec_yr) |> mean(na.rm = TRUE)),
    
    se_ba_loss_yr = map_dbl(data, ~pull(., ba_loss_yr) |> std_error(na.rm = TRUE)),
    se_ba_gain_yr = map_dbl(data, ~pull(., ba_gain_yr) |> std_error(na.rm = TRUE)),
    se_n_mor_yr = map_dbl(data, ~pull(., n_mor_yr) |> std_error(na.rm = TRUE)),
    se_n_rec_yr = map_dbl(data, ~pull(., n_rec_yr) |> std_error(na.rm = TRUE))
  )
```


```{r}
p <- 
  df_tmp2 |> 
  drop_na() |> 
  arrange(region, campagne_1) |> 
  mutate(campagne_2 = campagne_1 + 5) |> 
  select(-data) |> 
  ggplot() + 
  aes(y = mean_ba_gain_yr, 
      x = mean_ba_loss_yr,
      group = 1,
      fill = as.factor(campagne_2)) +
  # Add layout
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~region, scales = "fixed") + 
  
  # Add points
  geom_errorbar(
    aes(ymin = mean_ba_gain_yr - se_ba_gain_yr, 
        ymax = mean_ba_gain_yr + se_ba_gain_yr,
        color = as.factor(campagne_2))
    # width = 0.2  # Width of the error bars
  ) +
  geom_errorbarh(
    aes(xmin = mean_ba_loss_yr - se_ba_loss_yr, 
        xmax = mean_ba_loss_yr + se_ba_loss_yr,
        color = as.factor(campagne_2))
    # width = 0.2  # Width of the error bars
  ) +
  geom_point(color = "black", shape = 21, size = 3) + 
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  
  # Add arrow paths
  geom_path(aes(alpha = campagne_2),
            arrow = arrow(type = "closed", 
                          length = unit(0.08, "inches"))) +
  scale_alpha_continuous(range = c(0.5, 1)) +  # Adjust the range for desired transparency
  
  # Add theme
  labs(
    title = paste0("Basal Area Trajectories of ", selected_species),
    x = "Mean Basal Area Loss between two census [% / m^2 tree / ha land / yr]",
    y = "Mean Basal Area Gain between two census [% / m^2 tree / ha land / yr]",
    fill = "Year of 2nd Census"
  ) +
  # scale_x_continuous(breaks = c(0, 0.5, 1)) +
  # scale_y_continuous(breaks = c(0, 0.5, 1)) +
  # ylim(0, 1) +
  # xlim(0, 0.5) +
  theme_linedraw() +
  guides(alpha = FALSE, color = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# theme(legend.position = "right") +
# guides(alpha = FALSE, fill = guide_legend(nrow = 1)) 

ggsave(
  paste0(
    "figures/", format(Sys.time(), format = "%Y-%m-%d"), "/",
    format(Sys.time(), format = "%H%M%S"), "_plot_",
    selected_species, "_gain-versus-loss-region-level_ABSOLUTE-VALUES.pdf"),
  p,
  height = 10,
  width = 13
)
```


```{r}
p <- 
  df_tmp2 |> 
  drop_na() |> 
  arrange(region, campagne_1) |> 
  mutate(campagne_2 = campagne_1 + 5) |> 
  select(-data) |> 
  ggplot() + 
  aes(y = mean_n_rec_yr, 
      x = mean_n_mor_yr,
      group = 1,
      fill = as.factor(campagne_2)) +
  # Add layout
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~region) + 
  
  # Add arrow paths
  geom_path(aes(alpha = campagne_2),
            arrow = arrow(type = "closed", 
                          length = unit(0.08, "inches"))) +
  scale_alpha_continuous(range = c(0.5, 1)) +  # Adjust the range for desired transparency
  
  # Add points
  geom_point(color = "black", shape = 21, size = 2) + 
  scale_fill_viridis_d() +
  
  # Add theme
  labs(
    title = paste0("Number of Trees Trajectories of ", selected_species),
    x = "Mortality [% stems / yr]",
    y = "Recruitment [% stems / yr]",
    fill = "Year of 2nd Census"
  ) +
  # scale_x_continuous(breaks = c(0, 0.5, 1)) +
  # scale_y_continuous(breaks = c(0, 0.5, 1)) +
  ylim(0, 0.02) +
  xlim(0, 0.02) +
  theme_linedraw() +
  guides(alpha = FALSE) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())
# theme(legend.position = "right") +
# guides(alpha = FALSE, fill = guide_legend(nrow = 1)) 

ggsave(
  paste0(
    "figures/", format(Sys.time(), format = "%Y-%m-%d"), "/",
    format(Sys.time(), format = "%H%M%S"), "_plot_",
    selected_species, "_mortality-versus-recruitment-region-level.pdf"),
  p,
  height = 10,
  width = 13
)
```
```{r}
xxx <- 
  xxx |> 
  group_by(dep) |> 
  mutate(
    growth_ba_yr_ha_scaled = growth_ba_yr_ha / max(growth_ba_yr_ha),
    death_ba_yr_ha_scaled  = death_ba_yr_ha / max(death_ba_yr_ha)
  ) |> 
  ungroup()

xxx |> 
  drop_na() |> 
  arrange(dep, campagne_1) |> 
  mutate(campagne_2 = campagne_1 + 5) |> 
  select(-data) |> 
  ggplot() + 
  aes(y = growth_ba_yr_ha_scaled, 
      x = death_ba_yr_ha_scaled,
      group = 1,
      fill = as.factor(campagne_2)) +
  # Add layout
  geom_abline(intercept = 0, slope = 1, linetype = "dotted") +
  facet_wrap(~dep) + 
  
  # Add arrow paths
  geom_path(aes(alpha = campagne_2),
            arrow = arrow(type = "closed", 
                          length = unit(0.08, "inches"))) +
  scale_alpha_continuous(range = c(0.5, 1)) +  # Adjust the range for desired transparency
  
  # Add points
  geom_point(color = "black", shape = 21, size = 2) + 
  scale_fill_viridis_d() +
  
  # Add theme
  labs(
    title = paste0("Growth versus Mortality Trajectories of ", selected_species),
    x = "Scaled Mortality Rate",
    y = "Scaled Growth Rate",
    fill = "Year of 2nd Census"
  ) +
  scale_x_continuous(breaks = c(0, 0.5, 1)) +
  scale_y_continuous(breaks = c(0, 0.5, 1)) +
  theme_linedraw() +
  theme(legend.position = "top") +
  guides(alpha = FALSE, fill = guide_legend(nrow = 1)) 
```
