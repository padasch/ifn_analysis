from sklearn import dummy
from tqdm import tqdm
import rasterio
import pandas as pd
import numpy as np
import os
import geopandas as gpd

# ------------------------------------------------------------------------------------------

def extract_raster_values(tiff_file, variable_name, latitudes, longitudes, progress_bar=False, expected_crs=None):
    
    if expected_crs is None:
        print(f"\t🚧 WARNING: No CRS specified. Make sure inputed coordinates have matching CRS!.")
        print(f"\t Returning None!")
        return None
    
    # Open the TIFF file
    with rasterio.open(tiff_file) as src:
        # Print the CRS (Coordinate Reference System) for quality control purposes
        if not expected_crs in str(src.crs):
            print(f"\t🚧 WARNING: The CRS is {str(src.crs)} and not {expected_crs}. Make sure inputed coordinates have matching CRS!.")
            print(f"\t Returning None!")
            return None

        # Create empty lists to store the extracted values and coordinates
        raster_values = []
        latitudes_out = []
        longitudes_out = []

        # Iterate over the latitudes and longitudes with or without progress bar
        if progress_bar:
            dummy = tqdm(zip(latitudes, longitudes), total=len(latitudes))
        else:
            dummy = zip(latitudes, longitudes)
        
        for lat, lon in dummy:
            # Get the row and column indices corresponding to the latitude and longitude coordinates
            row, col = src.index(lon, lat)

            # Check if the row and column indices are within the raster extent
            if row < 0 or row >= src.height or col < 0 or col >= src.width:
                # print(
                #     f"\t 🚧 WARNING: The lat/lon {lat}/{lon} are outside the extent of the TIFF file."
                # )
                raster_values.append(np.nan)
            else:
                # Read the raster value at the specified indices
                raster_value = src.read(1)[row, col]
                raster_values.append(raster_value)

            # Append the coordinates to the respective lists
            latitudes_out.append(lat)
            longitudes_out.append(lon)

        # Create a dataframe with the coordinates and the extracted values
        df = pd.DataFrame(
            {
                variable_name: raster_values,
                "Latitude": latitudes_out,
                "Longitude": longitudes_out,
            }
        )

        return df

# ------------------------------------------------------------------------------------------
def parallel_raster_extraction(df_files, df_coords, progress_bar=False,verbose=True):
    
    # Get df_all
    df_all = df_coords[["idp", "x_fr", "y_fr"]]

    # Print working directory
    # print("Working directory: ", os.getcwd())

    for i in range(len(df_files)):
        # Print progress
        if verbose:
            print(
                f"\nGroup {df_files['group'].iloc[i]} \t | {i+1}/{len(df_files)} | {df_files['variables'].iloc[i]}.tif",
                end="\t",
            )

        # Extract values from raster
        df = extract_raster_values(
            df_files["files"].iloc[i],
            df_files["variables"].iloc[i],
            df_all["y_fr"],
            df_all["x_fr"],
            progress_bar=progress_bar,
        )

        # Merge with final df
        df = df.reset_index(drop=True)
        df_all = pd.concat([df_all, df[df_files["variables"].iloc[i]]], axis=1)

    return df_all


def extract_closest_soil_polygon_parallel(group_in, soil_sites):
    df_all = pd.DataFrame()

    # Loop over every location in the group
    for i in tqdm(range(len(group_in))):
        # for i in range(len(group_in)):

        # Get copy of soil data
        tmp_soils = soil_sites.copy().reset_index(drop=True)

        # Slice group_in at ith location
        df_ith = pd.DataFrame(group_in.iloc[i]).T.reset_index(drop=True)

        # Turn into geodataframe
        df_ith = gpd.GeoDataFrame(
            df_ith,
            geometry=gpd.points_from_xy(df_ith.x_fr, df_ith.y_fr),
            crs="EPSG:2154",
        )[["idp", "geometry"]]

        # Turn to meter projection
        df_ith.to_crs(epsg=3857, inplace=True)
        tmp_soils.to_crs(epsg=3857, inplace=True)

        # Calculate distances and find the minimum
        point_to_compare = df_ith.geometry[0]
        distances = tmp_soils.distance(point_to_compare, align=False)
        min_dist_index = distances.idxmin()

        # Extract the row with the minimum distance
        closest_row = pd.DataFrame(tmp_soils.loc[min_dist_index]).T
        df_ith["rmqs_distance"] = distances[min_dist_index]

        # print(f"----------------------------------")
        # print(f"point_to_compare idp {df_ith['idp'][0]}: ", end="\t")
        # print(point_to_compare)
        # print(
        #     f"closest point site_id {closest_row.iloc[0]['id_site']}:\t{closest_row.iloc[0]['geometry']} is {df_ith.iloc[0]['rmqs_distance']} away"
        # )
        # display(closest_row)

        # Reset index and concatenate (drop all geometry columns because they are in wrong EPSG)
        closest_row = closest_row.drop(columns=["geometry"]).reset_index(drop=True)
        df_ith = df_ith.reset_index(drop=True)

        df_ith = pd.concat([df_ith[["idp", "rmqs_distance"]], closest_row], axis=1)

        # Attach to df_all
        df_all = pd.concat([df_all, df_ith], axis=0)

        # if i == 3:
        #     break

    return df_all

def wrapper_for_large_files(group_in, tif_in, var_in, progress_bar = False):
    
    df = extract_raster_values(
        tiff_file = tif_in,
        variable_name = var_in, 
        latitudes=group_in["y_fr"], 
        longitudes=group_in["x_fr"],
        progress_bar=progress_bar
        )
    
    df = df.drop(columns=["Latitude", "Longitude"]).reset_index(drop=True)
    df = pd.concat([group_in, df], axis=1)
    
    return df

# ------------------------------------------------------------------------------------------
# FUNCTIONS TO EXTRACT EDO DATA
# ------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------
def parallel_edo_extraction(group_in, df_sites, progress_bar=False, debug=False, expected_crs=None):
    """
    Extracts EDO data for all sites in df_sites for all files in group_in.
    """
    # Create empty dataframe to store results
    df_all_dates = pd.DataFrame()

    # Loop over all files in group_in
    for i in tqdm(range(len(group_in)), disable=not progress_bar):
        # Get current file and date
        current_file = group_in.iloc[i]["file"]
        current_date = group_in.iloc[i]["date"]
        current_var = group_in.iloc[i]["variable"]

        # Get current sites, meaning sites for which the current date is within the start and end year
        current_sites = df_sites.copy().reset_index(drop=True)
        current_sites = current_sites.query(
            "start_year <= @current_date.year <= end_year"
        ).reset_index(drop=True)

        # Print progress
        if progress_bar:
            print(
                f"\nProgress: {i+1}/{len(group_in)} | Variable: {current_var} | Date: {current_date.strftime('%Y-%m-%d')}",
                end="\t",
            )

        # Extract raster values
        df = extract_raster_values(
            tiff_file=current_file,
            variable_name=current_var,
            latitudes=current_sites.y,
            longitudes=current_sites.x,
            progress_bar=False,
            expected_crs=expected_crs,
        )

        # Attach date to df
        df["date"] = current_date
        # Match extracted value with sites
        df = pd.concat([df[[current_var, "date"]], current_sites], axis=1)
        # Combine all data of current variable row-wise
        df_all_dates = pd.concat([df_all_dates, df])

        # Debugging
        if debug:
            if i == 10:
                break

    return df_all_dates

# ------------------------------------------------------------------------------------------
def seasonal_aggregation_per_site(df_in, current_var, fcts_to_apply):
    grouped = df_in.groupby("idp")  # Group by idp
    df_list = [group for name, group in grouped]  # Create list
    df_out = pd.DataFrame()  # Create empty dataframe for output

    for i in tqdm(range(len(df_list))):
        # for i in range(len(df_list)):
        current_group = df_list[i].copy()[
            ["idp", "date", "first_year", "season", current_var]
        ]
        current_idp = df_list[i].idp.unique()[0]

        df_i = get_seasonal_aggregates(
            df_in=current_group,
            timescale_days_to_months="fall cut-off",
            fcts_to_apply=fcts_to_apply,
            debug=False,
            verbose=False,
        )

        df_i["idp"] = current_idp
        df_i.insert(0, "idp", df_i.pop("idp"))

        df_out = pd.concat([df_out, df_i])

    return df_out

# ------------------------------------------------------------------------------------------
def full_extraction_per_edo_subfolder():
    pass

# ------------------------------------------------------------------------------------------
def get_seasonal_aggregates(
    df_in=None,
    timescale_days_to_months="fall cut-off",
    fcts_to_apply=None,
    verbose=False,
    debug=False,
):
    """
    Compute seasonal aggregates of variables in a given dataframe.
    ❗ Function calculates the five years following the `first year` variable in the dataframe.
    ❗ If the previous x years are needed, shift input `first_year` by x years.

    Args:
    - df_in: pandas DataFrame containing the data to aggregate.
    - timescale_days_to_months: str, optional. The timescale to use for aggregation. Default is "fall cut-off".
    - fcts_to_apply: list of str, optional. The functions to apply for aggregation. Default is ["mean", "std"].
    - verbose: bool, optional. Whether to print information about the number of variables created. Default is True.
    - debug: bool, optional. Whether to print debug information. Default is False.

    Returns:
    - df_outside: pandas DataFrame containing the seasonal aggregates of the variables in df_in.
    """
    # Imports
    import numpy as np
    import pandas as pd
    import datetime as dt
    from os import error

    # Checks
    if df_in is None:
        error("No dataframe provided.")
    
    supported_fcts = ['mean', 'std', 'median', 'max', 'min', 'range', 'sum', 'iqr']
    default_fcts = ['mean', 'std']
    
    if fcts_to_apply is None:
        print(f"No functions provided! Using default: {default_fcts}.",
              f"Supported functions are: {supported_fcts}")

    # Settings
    # timescale_days_to_months = "fall cut-off"
    # fcts_to_apply     = ['mean', 'std']
    # fcts_to_apply     = ['mean', 'std', 'median', 'max', 'min']

    first_year = df_in["first_year"].unique()[0]

    vars_to_aggregate = df_in.drop(
        columns=["date", "idp","SiteID", "season", "first_year"],
        errors="ignore",
    ).columns

    # Reduce dataframe to relevant time period
    if timescale_days_to_months == "fall cut-off":
        # Set first and last day of time period
        # fall cut-off means that the first year of impact starts in September
        cut_off_date = "-09-01"

        first_day = str(first_year) + cut_off_date
        last_day = str(first_year + 5) + cut_off_date

        df_filtered_daterange = df_in.query("date >= @first_day and date < @last_day")

    # Create output dataframe
    df_outside = pd.DataFrame(
        {"nan": [np.nan]}
    )  # For some reason, I need to add an NA entry to attach new values in the loop...
    i = 0  # Set counter to 0
    
    # Define dictionary with functions
    fct_dict = {'mean': np.nanmean, 'std': np.nanstd, 'median': np.nanmedian, 'max': np.nanmax, 'min': np.nanmin, 'range': range_func, 'sum': np.nansum, 'iqr': iqr_func}
    
    # Loop through functions
    for my_fct in fcts_to_apply:
        # print(my_fct)
        # Loop through variables
        for my_var in vars_to_aggregate:
            # print(my_var)
            df_tmp = df_filtered_daterange.groupby("season", observed=False)[my_var].agg(fct_dict[my_fct])

            # Loop through seasons
            for my_season in df_in["season"].unique():
                var_name = my_fct + "_of_" + my_var + "_in_" + my_season
                my_value = df_tmp[my_season]
                # print(var_name, ':', my_value)
                df_outside[var_name] = my_value

                i = i + 1

    if verbose:
        print(f"Number of variables created: {i}")

    # Drop NA column again
    df_outside = df_outside.drop(columns=["nan"])

    return df_outside

# Define custom aggregation functions
def range_func(x):
    return x.nanmax() - x.minnan()

def iqr_func(x):
    return x.nanquantile(0.75) - x.nanquantile(0.25)