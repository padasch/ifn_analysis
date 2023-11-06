# Define a function that applies `perform_wrangling_on_all_sites` to a group
def apply_func(group):
    name, data = group
    return perform_wrangling_on_all_sites(data)

# ------------------------------------------------------------------------------

def perform_wrangling_on_all_sites(my_group):
    
    import pandas as pd
    
    # Fix inputs
    first_year = my_group["first_year"].unique()
    df_tmp = my_group.copy()
    debug = False
    
    # Create empty dataframe to store results
    df_out = pd.DataFrame()

    # Load data frame of given site
    # if debug: print('\n > Running function: load_and_merge_files()...')
    # df_tmp = load_and_merge_files(site_id)
    # return df_tmp

    # Fix and attach variables
    if debug:
        print("\n > Running function: fix_and_attach_variables()...")
    df_tmp = fix_and_attach_variables(df_tmp)
    return df_tmp

    # Attach general temporal aggregates
    if debug:
        print("\n > Running function: get_seasonal_aggregates()...")
    df_seas = get_seasonal_aggregates(
        df_in=df_tmp,
        first_year=first_year,
        timescale_days_to_months="fall cut-off",
        fcts_to_apply=["mean", "std"],
    )
    # return df_seas

    # Get heat wave information
    if debug:
        print("\n > Running function: extract_heatwave_metrics()...")
    df_hw = extract_heatwave_metrics(
        df_in=df_tmp,
        threshold_temperature=30,
        threshold_days=3,
        variable_of_interest="mean_2m_air_temperature",
    )
    # return df_hw

    # Get frost event information
    if debug:
        print("\n > Running function: detect_frost_events()...")
    df_fe = detect_frost_events(first_year=first_year, df_in=df_tmp)
    # return df_fe

    df_out = pd.concat([df_out, df_fe, df_hw, df_seas], axis=1)
    return df_out

def fix_and_attach_variables(df):
    
    import pandas as pd
    import datetime as dt

    # Fix and attach variables
    # Correct temperature scales
    temperature_cols = [
        'mean_2m_air_temperature', 
        'maximum_2m_air_temperature',
        'minimum_2m_air_temperature',
        'dewpoint_2m_temperature'
        ]

    df[temperature_cols] = df[temperature_cols].apply(lambda x: x - 273.15)
    df['date'] = pd.to_datetime(df['date'])
    df = df.sort_values('date')

    # Attach season information
    # Define seasonal date ranges
    seasons = {
        'winter': (12, 1, 2),
        'spring': (3, 4, 5),
        'summer': (6, 7, 8),
        'fall':   (9, 10, 11)
    }

    # Function to attach season based on month
    def attach_season(date):
        month = date.month
        for season, months_range in seasons.items():
            if month in months_range:
                return season

    # Apply the function to create a new 'Season' column
    df['season'] = df['date'].apply(attach_season).astype('category')
    
    return df

def get_seasonal_aggregates(
    df_in,
    first_year,
    timescale_days_to_months="fall cut-off",
    fcts_to_apply=["mean", "std"],
):
    import numpy as np
    import pandas as pd
    import datetime as dt

    # Settings
    # timescale_days_to_months = "fall cut-off"
    # first_year               = df_single_site['first_visit'].iloc[0]
    # fcts_to_apply     = ['mean', 'std']
    # fcts_to_apply     = ['mean', 'std', 'median', 'max', 'min']

    vars_to_aggregate = df_in.drop(
        columns=["date", "SiteID", "season", "first_year"]
    ).columns

    # Reduce dataframe to relevant time period
    if timescale_days_to_months == "fall cut-off":
        # Set first and last day of time period
        cut_off_date = "-09-01"

        first_day = str(first_year) + cut_off_date
        last_day = str(first_year + 5) + cut_off_date

        df_filtered_daterange = df_in.query("date >= @first_day and date < @last_day")

    # Create output dataframe
    df_outside = pd.DataFrame(
        {"nan": [np.nan]}
    )  # For some reason, I need to add an NA entry to attach new values in the loop...
    i = 0  # Set counter to 0

    # Loop through functions
    for my_fct in fcts_to_apply:
        # print(my_fct)
        # Loop through variables
        for my_var in vars_to_aggregate:
            # print(my_var)
            df_tmp = df_filtered_daterange.groupby("season", observed=False)[
                my_var
            ].agg(my_fct)
            # print(df_tmp)
            # print(df_tmp['fall'])

            # Loop through seasons
            for my_season in ["summer", "spring", "fall", "winter"]:
                var_name = my_fct + "_of_" + my_var + "_in_" + my_season
                my_value = df_tmp[my_season]
                # print(var_name, ':', my_value)
                df_outside[var_name] = my_value

                i = i + 1

    if debug:
        print(f"Number of variables created: {i}")

    # Drop NA column again
    df_outside = df_outside.drop(columns=["nan"])

    return df_outside