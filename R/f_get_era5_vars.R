get_era5_vars <- function(subset){
  
  
  if (subset == "hourly") {
    
    out <- 
      c(
        # "dewpoint_temperature_2m"
        # "temperature_2m"
        # ,"skin_temperature"
        # ,"soil_temperature_level_1"
        # ,"soil_temperature_level_2"
        # ,"soil_temperature_level_3"
        # ,"soil_temperature_level_4"
        # ,"lake_bottom_temperature"
        # ,"lake_ice_depth"
        # ,"lake_ice_temperature"
        # ,"lake_mix_layer_depth"
        # ,"lake_mix_layer_temperature"
        # ,"lake_shape_factor"
        # ,"lake_total_layer_temperature"
        # ,"snow_albedo"
        # ,"snow_cover"
        # ,"snow_density"
        # ,"snow_depth"
        # ,"snow_depth_water_equivalent"
        # ,"snowfall"
        # ,"snowmelt"
        # ,"temperature_of_snow_layer"
        # ,"skin_reservoir_content"
        # ,"volumetric_soil_water_layer_1"
        # ,"volumetric_soil_water_layer_2"
        # ,"volumetric_soil_water_layer_3"
        # ,"volumetric_soil_water_layer_4"
        # ,"forecast_albedo"
        # ,"surface_latent_heat_flux"
        # ,"surface_net_solar_radiation"
        # ,"surface_net_thermal_radiation"
        # ,"surface_sensible_heat_flux"
        # ,"surface_solar_radiation_downwards"
        # ,"surface_thermal_radiation_downwards"
        # ,"evaporation_from_bare_soil"
        # ,"evaporation_from_open_water_surfaces_excluding_oceans"
        # ,"evaporation_from_the_top_of_canopy"
        # ,"evaporation_from_vegetation_transpiration"
        # ,"potential_evaporation"
        # ,"runoff"
        # ,"snow_evaporation"
        # ,"sub_surface_runoff"
        # ,"surface_runoff"
        "total_evaporation"
        # ,"u_component_of_wind_10m"
        # ,"v_component_of_wind_10m"
        # ,"surface_pressure"
        ,"total_precipitation"
        # ,"leaf_area_index_high_vegetation"
        # ,"leaf_area_index_low_vegetation"
        # ,"snowfall_hourly"
        # ,"snowmelt_hourly"
        # ,"surface_latent_heat_flux_hourly"
        # ,"surface_net_solar_radiation_hourly"
        # ,"surface_net_thermal_radiation_hourly"
        # ,"surface_sensible_heat_flux_hourly"
        # ,"surface_solar_radiation_downwards_hourly"
        # ,"surface_thermal_radiation_downwards_hourly"
        # ,"evaporation_from_bare_soil_hourly"
        # ,"evaporation_from_open_water_surfaces_excluding_oceans_hourly"
        # ,"evaporation_from_the_top_of_canopy_hourly"
        # ,"evaporation_from_vegetation_transpiration_hourly"
        # ,"potential_evaporation_hourly"
        # ,"runoff_hourly"
        # ,"snow_evaporation_hourly"
        # ,"sub_surface_runoff_hourly"
        # ,"surface_runoff_hourly"
        # ,"total_evaporation_hourly"
        # ,"total_precipitation_hourly
      )
    
  } else if (subset == "daily") {
    
    out <- 
      c(
        "mean_2m_air_temperature" 
        ,"minimum_2m_air_temperature" 
        ,"maximum_2m_air_temperature" 
        # ,"dewpoint_2m_temperature" 
        # ,"total_precipitation" 
        # ,"surface_pressure" 
        # ,"mean_sea_level_pressure" 
        # ,"u_component_of_wind_10m" 
        # ,"v_component_of_wind_10m" 
      )
    } else {
    stop("Wrong subset selection.")
  }
  
  return(out)
}
