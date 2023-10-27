# FRIDAY 2023-10-27 ----

# S2 Wrapping working function

roi <- gee_get_france_boundaries()

# FUNCTIONS
# Create mask to filter out cloudy pictures
maskS2clouds <- function(image){
  qa <- image$select("QA60")
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask  <-  bitwShiftL(1, 10)
  cirrusBitMask <-  bitwShiftL(1, 11)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  
  return(image$updateMask(mask)$divide(10000))
}

# Add date property to images
addDate <- function(image){
  
  img_date <- ee$Date(image$date())
  img_date <- ee$Number$parse(img_date$format('YYYYMMdd'))
  out <- image$addBands(ee$Image(img_date)$rename('system:time_start')$toInt())
  
  return(out)
}


# EVI
getEVI <- function(image){
  
  # Compute the EVI using an expression.
  # No need to divide by 10^5 again, because is done in cloud masking!
  EVI <- image$expression(
    '2.5 * ((NIR - RED) / (NIR + 6 * RED - 7.5 * BLUE + 1))',
    list(
      'NIR' = image$select('B8'), 
      'RED' = image$select('B4'),
      'BLUE'= image$select('B2')
    ))$rename("EVI")
  
  image <- image$addBands(EVI)
  
  return(image)
}

# NDVI
getNDVI <- function(image){
  # Compute the NDVI using an expression.
  # No need to divide by 10^5 again, because is done in cloud masking!
  EVI <- image$expression(
    '(NIR - RED) / (NIR + RED)',
    list(
      'NIR' = image$select('B8'),
      'RED' = image$select('B4')
    ))$rename("NDVI")
  
  image <- image$addBands(EVI)
  
  return(image)
}

# Map the function over a month of data and take the median.
# Load Sentinel-2 TOA reflectance data (adjusted for processing changes
# that occurred after 2022-01-25).
# Pre-filter to get less cloudy granules.
sentinel  <- 
  ee$
  ImageCollection('COPERNICUS/S2_HARMONIZED')$
  filterDate('2022-01-01', '2022-01-31')$
  filterBounds(roi)$
  filterMetadata("CLOUDY_PIXEL_PERCENTAGE", "less_than", 20)$
  map(maskS2clouds)$
  # map(addDate)$
  map(getEVI)$
  map(getNDVI)$
  median() # For some reason, people online prefer median instead of mean

sentinel_clip <- sentinel$clip(roi)

NDVI <- sentinel_clip$select("NDVI")
EVI  <- sentinel_clip$select("EVI")

rgee_tasks[[my_var]][i] <- 
  list(
    ee_image_to_drive(
      image = NDVI,
      fileFormat = "GEO_TIFF",
      region = roi,
      scale  = 500,
      crs    = "EPSG:2154",
      description = paste0("NDVI Test"),
      folder = paste0("rgee/ifna/", format(Sys.time(), "%Y-%m-%d")),
      fileNamePrefix = my_filename,
      timePrefix = FALSE
    )
  )

# Start task
rgee_filenames[[c]] <- my_filename
rgee_tasks[[my_var]][[i]]$start()
ee_monitoring(eeTaskList = TRUE)



my_colors <- list(
  "min" = -1,
  "max" =  1,
  "palette" = list("red", "white", "green")
)

# Map$setCenter(roi)
# Map$setCenter(5, 45, 5)
# Map$addLayer(roi, list(), 'ROI')
Map$addLayer(NDVI, my_colors, 'NDVI')
Map$addLayer(EVI,  my_colors, 'EVI')
Map$addLayer(EVI,  my_colors, 'EVI')
# Map$addLayer(sentinel, rgbVis, 'RGB')


# Cloud filtering routine from S2 online description
# JAVASCRIPT VERSION, COPIED FROM HERE https://developers.google.com/earth-engine/datasets/catalog/COPERNICUS_S2_HARMONIZED
# /**
#   * Function to mask clouds using the Sentinel-2 QA band
# * @param {ee.Image} image Sentinel-2 image
# * @return {ee.Image} cloud masked Sentinel-2 image
# */
#   function maskS2clouds(image) {
#     var qa = image.select('QA60');
#     
#     // Bits 10 and 11 are clouds and cirrus, respectively.
#     var cloudBitMask = 1 << 10;
#     var cirrusBitMask = 1 << 11;
#     
#     // Both flags should be set to zero, indicating clear conditions.
#     var mask = qa.bitwiseAnd(cloudBitMask).eq(0)
#     .and(qa.bitwiseAnd(cirrusBitMask).eq(0));
#     
#     return image.updateMask(mask).divide(10000);
#   }
# 
# // Map the function over a month of data and take the median.
# // Load Sentinel-2 TOA reflectance data (adjusted for processing changes
#                                          // that occurred after 2022-01-25).
# var dataset = ee.ImageCollection('COPERNICUS/S2_HARMONIZED')
# .filterDate('2022-01-01', '2022-01-31')
# // Pre-filter to get less cloudy granules.
# .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
# .map(maskS2clouds);
# 
# var rgbVis = {
#   min: 0.0,
#   max: 0.3,
#   bands: ['B4', 'B3', 'B2'],
# };
# 
# Map.setCenter(-9.1695, 38.6917, 12);
# Map.addLayer(dataset.median(), rgbVis, 'RGB');

# R VERSION WORKING
#' Function to mask clouds using the Sentinel-2 QA band
#' @param {ee.Image} image Sentinel-2 image
#' @return {ee.Image} cloud masked Sentinel-2 image

maskS2clouds <- function(image){
  qa <- image$select("QA60")
  
  # Bits 10 and 11 are clouds and cirrus, respectively.
  cloudBitMask  <-  bitwShiftL(1, 10)
  cirrusBitMask <-  bitwShiftL(1, 11)
  
  # Both flags should be set to zero, indicating clear conditions.
  mask <- qa$bitwiseAnd(cloudBitMask)$eq(0)$And(qa$bitwiseAnd(cirrusBitMask)$eq(0))
  
  return(image$updateMask(mask)$divide(10000))
}

# Map the function over a month of data and take the median.
# Load Sentinel-2 TOA reflectance data (adjusted for processing changes
# that occurred after 2022-01-25).
# Pre-filter to get less cloudy granules.

dataset  <- 
  ee$
  ImageCollection('COPERNICUS/S2_HARMONIZED')$
  filterDate('2022-01-01', '2022-01-31')$
  filter(ee$Filter$lt('CLOUDY_PIXEL_PERCENTAGE', 20))$
  map(maskS2clouds)

rgbVis  <- list(
  "min"   = 0.0,
  "max"   = 0.3,
  "bands" = list('B4', 'B3', 'B2')
)

Map$setCenter(-9.1695, 38.6917, 12)
Map$addLayer(dataset$median(), rgbVis, 'RGB')

# ______________________________________________________________________________
date_start <- "2022-03-01"
date_end   <- "2022-06-01"
my_data    <- "COPERNICUS/S2"
roi        <- gee_get_france_boundaries()

S2 <- 
  ee$
  ImageCollection(my_data)$
  filterBounds(roi)$
  filterDate(date_start, date_end)$
  mean() # For some reason, people online prefer median instead of mean

# Map$addLayer(roi)
# Map$centerObject(roi)
Map$addLayer(S2)

# THURSDAY 2023-10-26 ----
# Comparison of MODIS and S2 outputs

date_start <- "2022-03-01"
date_end   <- "2022-06-01"
my_data    <- "COPERNICUS/S2"

S2 <- 
  ee$
  ImageCollection(my_data)$
  filterBounds(roi)$
  filterDate(date_start, date_end)$
  mean() # For some reason, people online prefer median instead of mean

NIR  <- S2$select('B8')$divide(1000)
RED  <- S2$select('B4')$divide(1000)
BLU  <- S2$select('B2')$divide(1000)

NDVI <- NIR$subtract(RED)$divide(NIR$add(RED))$rename("NDVI")
NDVI <- NDVI$clip(roi)

# EVI Equation from here: https://custom-scripts.sentinel-hub.com/custom-scripts/sentinel-2/evi/
EVI  <- 
  NIR$subtract(RED)$multiply(2.5)$
  divide(NIR$add(RED$multiply(6.0)$subtract(BLU$multiply(7.5))$add(1.0)))

EVI <- EVI$rename("EVI")$clip(roi)

# image <- EVI
# image <- NDVI

my_data    <- "MODIS/061/MOD13A1"
modis_ndvi <- 
  ee$
  ImageCollection(my_data)$
  select("NDVI")$
  filterBounds(roi)$
  filterDate(date_start,
             date_end)$
  mean()

modis_evi <- 
  ee$
  ImageCollection(my_data)$
  select("EVI")$
  filterBounds(roi)$
  filterDate(date_start,
             date_end)$
  mean()


# ______________________________________________________________________________
# JAVASCRIPT VERSION

var date_start = "2022-03-01"
var date_end   = "2022-06-01"
var my_data    = "COPERNICUS/S2"

var S2 = ee
.ImageCollection(my_data)
.filterBounds(roi)
.filterDate(date_start, date_end)
.mean();

var NIR = S2.select('B8').divide(1000);
var RED = S2.select('B4').divide(1000);
var BLU = S2.select('B2').divide(1000);

var NDVI =NIR.subtract(RED).divide(NIR.add(RED)).rename("NDVI");
var NDVI =NDVI.clip(roi);

var EVI  = NIR.subtract(RED).multiply(2.5).divide(NIR.add(RED.multiply(6.0).subtract(BLU.multiply(7.5)).add(1.0)));

var EVI = EVI.rename("EVI").clip(roi);

var my_data = "MODIS/061/MOD13A1";
var modis_ndvi  = ee
.ImageCollection(my_data)
.select("NDVI")
.filterBounds(roi)
.filterDate(date_start,
            date_end)
.mean();

var modis_evi = ee
.ImageCollection(my_data)
.select("EVI")
.filterBounds(roi)
.filterDate(date_start,
            date_end)
.mean();


# BEFORE ----
# ______________________________________________________________________________
roi <- gee_get_france_boundaries()

first_year <- "2009"
last_year  <- "2010"

my_data  <- "ECMWF/ERA5/DAILY"
my_var <- "mean_2m_air_temperature"



# Tutorial to get daily ndvi at a location
# ______________________________________________________________________________
# rgee setup
source("R/_setup.R")
library(rgee)

# Running the command below requires a restart of R but then rgee should work fine.
rgee_environment_dir <- "/Users/pascal/anaconda3/envs/rgee_py/bin"
rgee::ee_install_set_pyenv(
  py_path = rgee_environment_dir, # Change it for your own Python PATH
  py_env = "rgee_py" # Change it for your own Python ENV
)

# ee_clean_user_credentials() # Redo this if KeyError pops up
rgee::ee_Initialize(drive = T)

# Get time of interest
first_day <- "2010-01-01"
last_day  <- "2011-01-01"

# Get locations
load_or_save_latest_file(nfi_dataset_for_analysis, "load")
df_xy <- 
  get_coords_from_df(nfi_dataset_for_analysis, "lon", "lat") |> 
  mutate(row_id = row_number()) |> 
  st_as_sf(coords = c("x", "y")) |> 
  st_set_crs(4326)

sites <- ee$FeatureCollection(df_xy)

# Get collection
modis       <- ee$FeatureCollection("MODIS/061/MOD13A2")
modis_dates <- modis$ee$
  modis_ndvi <- 

# 2023-10-06 ----
## Trajectory Code ----

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
# ______________________________________________________________________________
# 2023-10-23 ----
## Scatterplot / LM / Distrib Plots for Shiny ----
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

my_x     <- sample(var_fac, 1)
my_y     <- sample(var_fac, 1)
my_group <- sample(var_fac, 1)

while (my_x == my_y) {my_y <- sample(var_num, 1)}

my_x <- "age13"
my_y <- "htot"
my_group <- "genus_lat"

# df_plot ----
## WITHOUT GROUPING VARIABLE -----
df_plot <- 
  nfi_dataset_for_analysis |> 
  mutate(
    x = get(my_x),
    y = get(my_y)
  ) |> 
  select(x, y) |> 
  drop_na(x, y)

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
  rename(g = Var1, n = Freq)

df_plot <- 
  df_plot |> 
  filter(g %in% top_10$g) |> 
  left_join(top_10, by = join_by(g)) |> 
  mutate(g = as.factor(paste0(g, " (N = ", n,")")))

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
    scale = 0.85,
    calc_ecdf = TRUE,
    quantiles = 4, 
    quantile_lines = TRUE, 
    jittered_points = TRUE,
    position = position_points_jitter(width = 0.05, height = 0, yoffset = -0.05),
    point_shape = '|', 
    point_size = 2, 
    point_alpha = 0.75, 
    alpha = 0.7
  ) +
  scale_fill_viridis_d(name = "Quartiles") +
  labs(
    x = my_y,
    y = my_group,
    color = my_group,
    fill = my_group
  ) +
  theme_classic()

df_plot |> 
  ggplot() +
  aes(y = g) +
  geom_bar()

# ______________________________________________________________________________
