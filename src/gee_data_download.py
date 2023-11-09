from gee_subset import gee_subset
from warnings import warn
from datetime import datetime
from pyprojroot.here import here

import os, re

import warnings
import pandas as pd
import geopandas as gpd
import ee

# ee.Authenticate()
ee.Initialize()

# ----------------------------------------------------------------
def get_location_site_df():
    return pd.read_csv(here("python/00_process_nfi_data/nfi_final_sites.csv"))

# ----------------------------------------------------------------
def adjust_first_last_date(df, plus_years, minus_years, first_date, last_date):
    """
    Adjusts the first and last date of a dataframe based on the 'first_year' column and given parameters.
    
    Args:
    - df: pandas dataframe with a 'first_year' column
    - plus_years: integer, number of years to add to 'first_year' for the last date
    - minus_years: integer, number of years to subtract from 'first_year' for the first date
    - first_date: string, date in format 'MM-dd' to add to the calculated first date
    - last_date: string, date in format 'MM-dd' to add to the calculated last date
    
    Returns:
    - df: pandas dataframe with adjusted 'first_date' and 'last_date' columns, and 'first_year' column dropped
    """
        
    # Convert 'first_year' to integer for calculation
    df['first_year'] = df['first_year'].astype(int)

    # Add first and last date
    df['first_date'] = (df['first_year'] - minus_years).astype(str) + first_date
    df['last_date']  = (df['first_year'] + plus_years).astype(str)  + last_date

    # Drop first_year
    df = df.drop(columns=['first_year'])
    
    return df

def smaller_date(date_str1, date_str2):
    """
    Returns the smaller of two dates in string format.

    Args:
        date_str1 (str): A date string in the format '%Y-%m-%d'.
        date_str2 (str): A date string in the format '%Y-%m-%d'.

    Returns:
        str: The smaller of the two dates in string format.
    """
    
    # Convert strings to datetime objects
    date1 = datetime.datetime.strptime(date_str1, '%Y-%m-%d')
    date2 = datetime.datetime.strptime(date_str2, '%Y-%m-%d')

    # Find the smaller date
    smaller_date = min(date1, date2)
    smaller_date_str = smaller_date.strftime('%Y-%m-%d')

    return smaller_date_str

# ----------------------------------------------------------------
import datetime

def larger_date(date_str1, date_str2):
    """
    Returns the larger of two dates in string format.

    Args:
        date_str1 (str): A date string in the format '%Y-%m-%d'.
        date_str2 (str): A date string in the format '%Y-%m-%d'.

    Returns:
        str: The larger of the two dates in string format.
    """
    # Convert strings to datetime objects
    date1 = datetime.datetime.strptime(date_str1, '%Y-%m-%d')
    date2 = datetime.datetime.strptime(date_str2, '%Y-%m-%d')

    # Find the larger date
    larger_date = max(date1, date2)
    larger_date_str = larger_date.strftime('%Y-%m-%d')

    return larger_date_str

# ----------------------------------------------------------------
def download_gee_data(
    siteSet,
    skip_to_i,
    product,
    my_bands,
    data_clean,
    product_start_date,
    product_end_date,
    product_scale,
    output_scale,
    output_folder,
    ):
    """
    Downloads Google Earth Engine data for a set of sites and saves it to disk.

    Args:
        siteSet (set): A set of site IDs to download data for.
        skip_to_i (int): The index of the first site to download data for.
        product (str): The name of the Google Earth Engine product to download.
        my_bands (list): A list of band names to download.
        data_clean (pandas.DataFrame): A DataFrame containing site information.
        product_start_date (str): The start date for the product data to download.
        product_end_date (str): The end date for the product data to download.
        product_scale (int): The scale of the product data to download.
        output_scale (int): The scale of the output data to save.
        output_folder (str): The path to the folder where the output data will be saved.
    """
    
    # Create folder if it doesn't exist
    if not os.path.exists(output_folder):
        os.makedirs(output_folder)

    for i in siteSet:

        if i < skip_to_i:
            continue
        
        df_loop = gee_subset.gee_subset(
            product    = product, 
            bands      = my_bands, 
            start_date = larger_date(data_clean.iloc[i, 3],  product_start_date), 
            end_date   = smaller_date(data_clean.iloc[i, 4], product_end_date), 
            latitude   = data_clean.iloc[i, 2], 
            longitude  = data_clean.iloc[i ,1], 
            scale      = max(product_scale, output_scale)
            )

        # Attach site id for joining later
        df_loop["SiteID"] = str(data_clean.iloc[i, 0]) 
        
        # Remove unnecessary columns to save space and time
        df_loop = df_loop.drop(columns=['id', 'longitude', 'latitude', 'product'])
        
        # Write to csv file
        #   df_loop.to_csv(output_folder + "/site_" + str(data_clean.iloc[i, 0]) + ".csv", index = False)
        df_loop.to_feather(output_folder + "/site_" + str(data_clean.iloc[i, 0]) + ".feather")

# ----------------------------------------------------------------
def download_gee_data_PARALLEL(
    my_group,
    product,
    my_bands,
    product_start_date,
    product_end_date,
    product_scale,
    output_scale,
    output_folder,
    verbose = False,):        
    """
    Downloads Google Earth Engine data for a given set of sites in parallel.
    
    Args:
    - my_group: pandas DataFrame containing site information
    - product: string, name of the GEE product to download
    - my_bands: list of strings, names of the bands to download
    - product_start_date: string, start date of the product to download
    - product_end_date: string, end date of the product to download
    - product_scale: int, scale of the product to download
    - output_scale: int, scale of the output data
    - output_folder: string, path to the folder where the output data will be saved
    - verbose: bool, whether to print progress messages
    
    Returns:
    None
    """     
    
    warnings.simplefilter(action="ignore", category=FutureWarning)

    
    # Create folder if it doesn't exist
    if not os.path.exists(output_folder): os.makedirs(output_folder)

    for i in range(0, len(my_group)):
        
        if verbose: warn(f'Working on site: {my_group.iloc[i, 0]}')
        
        df_loop = gee_subset.gee_subset(
            product    = product, 
            bands      = my_bands, 
            start_date = larger_date(my_group.iloc[i, 3],  product_start_date), 
            end_date   = smaller_date(my_group.iloc[i, 4], product_end_date), 
            latitude   = my_group.iloc[i, 2], 
            longitude  = my_group.iloc[i ,1], 
            scale      = max(product_scale, output_scale)
            )

        # Attach site id for joining later
        df_loop["SiteID"] = str(my_group.iloc[i, 0]) 
        
        # Remove unnecessary columns to save space and time
        df_loop = df_loop.drop(columns=['id', 'longitude', 'latitude', 'product'])
        
        # Write to csv file
        #   df_loop.to_csv(output_folder + "/site_" + str(my_group.iloc[i, 0]) + ".csv", index = False)
        df_loop.to_feather(output_folder + "/site_" + str(my_group.iloc[i, 0]) + ".feather")

# ----------------------------------------------------------------
def download_data_for_year(current_year, data_clean, siteSet, output_folder, product, my_bands, product_start_date, product_end_date, product_scale, output_scale):

    '''
    Function to download data per year and band for ERA5 daily.
    This is needed to avoid reaching user memory limit.
    '''

    output_folder_yr = output_folder + '/' + str(current_year)
    current_start = str(current_year) + '-01-01'
    current_end   = str(current_year + 1) + '-01-01'
    
    # Create folder if it doesn't exist
    # output_folder_yr_band = output_folder_yr + '/' + band
    if not os.path.exists(output_folder_yr):
        os.makedirs(output_folder_yr)

    # Start loop over bands
    for band in my_bands:
            
        # print('\014 Working on: ', band)
            
        # Start loop over all the data
        for i in siteSet:
            
            # Check if site needs data from current year
            i_year_0 = datetime.strptime(data_clean.iloc[i, 3], "%Y-%m-%d").year
            i_year_1 = datetime.strptime(data_clean.iloc[i, 4], "%Y-%m-%d").year
            
            if  i_year_0 <= current_year <= i_year_1:
            
                # Skip to site i, if required
                if i < skip_to_i:
                    continue
            
                df_loop = gee_subset.gee_subset(
                    product    = product, 
                    bands      = [band], 
                    start_date = larger_date(current_start, product_start_date), 
                    end_date   = smaller_date(current_end, product_end_date), 
                    latitude   = data_clean.iloc[i, 2], 
                    longitude  = data_clean.iloc[i, 1], 
                    scale      = max(product_scale, output_scale)
                    )

                sid = str(data_clean.iloc[i, 0]) 
                df_loop["SiteID"] = sid
                df_loop = df_loop.drop(columns=['id', 'longitude', 'latitude', 'product'])
                df_loop.to_csv(output_folder_yr + '/' + band + "_site_" + str(data_clean.iloc[i, 0]) + ".csv")
            else:
                continue