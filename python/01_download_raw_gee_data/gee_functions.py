# ----------------------------------------------------------------
def import_libraries():
    # Import Libraries
    import ee
    # ee.Authenticate()
    ee.Initialize()

    import os, re
    import pandas as pd
    from gee_subset import gee_subset
    import geopandas as gpd
    from datetime import datetime

# ----------------------------------------------------------------
def get_location_site_df():
    return pd.read_csv("sites_years.csv")

# ----------------------------------------------------------------
def adjust_first_last_date(df, plus_years, minus_years, first_date, last_date):
    
    # Convert 'first_visit' to integer for calculation
    df['first_visit'] = df['first_visit'].astype(int)

    # Add first and last date
    df['first_date'] = (df['first_visit'] - minus_years).astype(str) + first_date
    df['last_date']  = (df['first_visit'] + plus_years).astype(str)  + last_date

    # Drop first_visit
    df = df.drop(columns=['first_visit'])
    
    return df

def smaller_date(date_str1, date_str2):

    # Convert strings to datetime objects
    date1 = datetime.strptime(date_str1, '%Y-%m-%d')
    date2 = datetime.strptime(date_str2, '%Y-%m-%d')

    # Find the smaller date
    smaller_date = min(date1, date2)
    smaller_date_str = smaller_date.strftime('%Y-%m-%d')

    return smaller_date_str

# ----------------------------------------------------------------
def larger_date(date_str1, date_str2):

    # Convert strings to datetime objects
    date1 = datetime.strptime(date_str1, '%Y-%m-%d')
    date2 = datetime.strptime(date_str2, '%Y-%m-%d')

    # Find the smaller date
    smaller_date = max(date1, date2)
    smaller_date_str = smaller_date.strftime('%Y-%m-%d')

    return smaller_date_str

# ----------------------------------------------------------------
def download_gee_data(
    siteSet,
    skip_to_i,
    product,
    my_bands,
    data_clean,
    product_start_date,
    product_scale,
    output_scale,
    output_folder
):
        
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
def download_data_for_year(current_year, data_clean, siteSet, output_folder, product, my_bands, product_scale, output_scale):

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