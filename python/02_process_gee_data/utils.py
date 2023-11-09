import imp
from logging import warn


def load_and_merge_files(site_id, first_year, subdir, verbose=True):
    """
    Load all data from subdirectory with format site_*.feather and merge into one dataframe.

    Returns:
        dataframe: holding all data from subdirectory
    """

    import pandas as pd
    import glob

    # Specify the folder path and file extension
    folder_path = "../01_download_raw_gee_data/gee-raw-data/" + subdir + "/*"
    file_extension = ("site_" + str(site_id) + ".feather")  # Change this to desired file extension

    if verbose:
        print(f"Loading file:\t {file_extension}")

    # Use glob to get a list of file paths
    file_paths = glob.glob(f"{folder_path}/{file_extension}")

    if len(file_paths) > 0:

        # Read all files into a list of DataFrames
        dfs = [pd.read_feather(file) for file in file_paths]

        # Merge dataframes into one
        df_out = dfs[0]  # Start with the first DataFrame
        for df in dfs[1:]:
            df_out = pd.merge(df_out, df, on=["date", "SiteID"])

        # Attach site ID and first year
        df_out["SiteID"] = site_id
        df_out["first_year"] = first_year

        return df_out

# ------------------------------------------------------------------------------

def load_and_wrangle_PARALLEL(my_group, subdir, verbose=True):
    """
    Load all data from subdirectory with format site_*.feather and merge into one dataframe.

    Returns:
        dataframe: holding all data from subdirectory
    """

    import pandas as pd
    import glob
    
    site_id    = my_group["id"].iloc[0]
    first_year = my_group["first_visit"].iloc[0]

    # Specify the folder path and file extension
    folder_path = "../01_download_raw_gee_data/gee-raw-data/" + subdir + "/*"
    file_extension = (
        "site_" + str(site_id) + ".feather"
    )  # Change this to your desired file extension

    if verbose:
        print(f"Loading file:\t {file_extension}")

       # Use glob to get a list of file paths
    file_paths = glob.glob(f"{folder_path}/{file_extension}")

    if len(file_paths) > 0:

        # Read all files into a list of DataFrames
        dfs = [pd.read_feather(file) for file in file_paths]

        # Merge dataframes into one
        df_out = dfs[0]  # Start with the first DataFrame
        for df in dfs[1:]:
            df_out = pd.merge(df_out, df, on=["date", "SiteID"])

        # Attach site ID and first year
        df_out["SiteID"] = site_id
        df_out["first_year"] = first_year

        df_out = perform_wrangling_on_all_sites(df_out)
        return df_out
    else:
        warn(f"No files found for site {site_id} in {subdir}.")


# ------------------------------------------------------------------------------
def fix_and_attach_variables(df):
    import pandas as pd
    import datetime as dt

    # Fix and attach variables
    # Correct temperature scales
    temperature_cols = [
        "mean_2m_air_temperature",
        "maximum_2m_air_temperature",
        "minimum_2m_air_temperature",
        "dewpoint_2m_temperature",
    ]

    df[temperature_cols] = df[temperature_cols].apply(lambda x: x - 273.15)
    df["date"] = pd.to_datetime(df["date"])
    df = df.sort_values("date")

    # Attach season information
    # Define seasonal date ranges
    seasons = {
        "winter": (12, 1, 2),
        "spring": (3, 4, 5),
        "summer": (6, 7, 8),
        "fall": (9, 10, 11),
    }

    # Function to attach season based on month
    def attach_season(date):
        month = date.month
        for season, months_range in seasons.items():
            if month in months_range:
                return season

    # Apply the function to create a new 'Season' column
    df["season"] = df["date"].apply(attach_season).astype("category")

    return df


# ------------------------------------------------------------------------------
def get_seasonal_aggregates(
    df_in,
    timescale_days_to_months="fall cut-off",
    fcts_to_apply=["mean", "std"],
    debug=False,
    verbose=True,
):
    import numpy as np
    import pandas as pd
    import datetime as dt

    # Settings
    # timescale_days_to_months = "fall cut-off"
    # fcts_to_apply     = ['mean', 'std']
    # fcts_to_apply     = ['mean', 'std', 'median', 'max', 'min']

    # Handle input variables
    first_year = df_in["first_year"].unique()[0]

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

    if verbose:
        print(f"Number of variables created: {i}")

    # Drop NA column again
    df_outside = df_outside.drop(columns=["nan"])

    return df_outside


# ------------------------------------------------------------------------------
# Define a function that applies `perform_wrangling_on_all_sites` to a group
def apply_func(group):
    name, data = group
    return perform_wrangling_on_all_sites(data)


# ------------------------------------------------------------------------------
def fix_and_attach_variables(df):
    import pandas as pd
    import datetime as dt

    # Fix and attach variables
    # Correct temperature scales
    temperature_cols = [
        "mean_2m_air_temperature",
        "maximum_2m_air_temperature",
        "minimum_2m_air_temperature",
        "dewpoint_2m_temperature",
    ]

    df[temperature_cols] = df[temperature_cols].apply(lambda x: x - 273.15)
    df["date"] = pd.to_datetime(df["date"])
    df = df.sort_values("date")

    # Attach season information
    # Define seasonal date ranges
    seasons = {
        "winter": (12, 1, 2),
        "spring": (3, 4, 5),
        "summer": (6, 7, 8),
        "fall": (9, 10, 11),
    }

    # Function to attach season based on month
    def attach_season(date):
        month = date.month
        for season, months_range in seasons.items():
            if month in months_range:
                return season

    # Apply the function to create a new 'Season' column
    df["season"] = df["date"].apply(attach_season).astype("category")

    return df


def get_seasonal_aggregates(
    df_in,
    timescale_days_to_months="fall cut-off",
    fcts_to_apply=["mean", "std"],
    verbose=True,
    debug=False,
):
    import numpy as np
    import pandas as pd
    import datetime as dt

    # Settings
    # timescale_days_to_months = "fall cut-off"
    # fcts_to_apply     = ['mean', 'std']
    # fcts_to_apply     = ['mean', 'std', 'median', 'max', 'min']
    
    first_year = df_in['first_year'].unique()[0]

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

    if verbose:
        print(f"Number of variables created: {i}")

    # Drop NA column again
    df_outside = df_outside.drop(columns=["nan"])

    return df_outside

# ------------------------------------------------------------------------------

def extract_min_days_between_extremes(time_series, df_tmp, threshold_days, verbose = False):
    """
    Algorithm to calculate the smallest time difference between two extremes.
    
    IMPORTANT: With the cleaned time-series of 0 and 1s, the patterns for the
    start and end of an event is imply 10 (end of event) and 01 (start of event).
    """
    
    import numpy as np
    import pandas as pd
    
    if verbose:
        print("> Calling extract_min_days_between_extremes:")

    all_days_between_heatwaves = []
    last_day_found = False

    for i in range(len(time_series) - 1):
        # Detect last day of heat wave
        if (time_series[i] >= threshold_days) & (time_series[i + 1] == 0):
            last_day = df_tmp.loc[i, "date"]
            last_day_found = True
            # if verbose: print(f"i = {i} \t i+1 = {i + 1}")
            # if verbose: print(f"time_series: i = {time_series[i]} \t i+1 = {time_series[i+1]}")
            if verbose:
                print(f"Last event ended on: {last_day} (i = {i}) ")

        # If a last day of an event was found, check if a next event can be found
        if last_day_found and i != 0:
            # Detect first day of heat wave
            if (time_series[i] == 0) & (time_series[i + 1] == 1):
                first_day = df_tmp.loc[i + 1, "date"]
                # print(time_series[i], "\t", time_series[i+1])

                # Update interval between last and first days
                days_between = (first_day - last_day).days - 1
                all_days_between_heatwaves.append(days_between)
                if verbose:
                    print(f"Next event started on: {first_day} (i = {i}) ")
                if verbose:
                    print(f"Time between these events: {days_between}")
                if verbose:
                    print("\n")

    # Drop negative values and 0s (errors in algorithm)
    all_days_between_heatwaves = [num for num in all_days_between_heatwaves if num > 0]

    # Find the minimum positive value
    if len(all_days_between_heatwaves) == 0:
        out = np.nan
    else:
        out = min(all_days_between_heatwaves)

    return out

# ------------------------------------------------------------------------------

def extract_heatwave_metrics(
    df_in, 
    threshold_temperature,
    threshold_days,
    variable_of_interest,
    verbose=False,
):
    
    import pandas as pd
    import numpy as np
    import re
    
    # if verbose: print('> Running extract_heatwave_metrics()...')

    # -------------------------------------------------------------------------------------------
    # Copy temporary dataframe and rename column of interest
    df_tmp = df_in.copy()
    df_tmp = df_tmp.rename(columns={variable_of_interest: "voi"})

    # DEBUG: Add artificial heatwaves to check for
    # use_value = 31
    # df_tmp.loc[0:1, 'voi'] = use_value # Start of timeseries
    # df_tmp.loc[5:10, 'voi'] = use_value # Middle of timeseries
    # df_tmp.loc[15:17, 'voi'] = use_value # Middle of timeseries
    # df_tmp.loc[df_tmp.index[-2:], 'voi'] = use_value # End of time series
    # Check if heatwaves were added
    # px.line(df_tmp, x="date", y=voi).show()
    # df_above30 = df_tmp.query('voi > @threshold_temperature')[['date', 'voi']]
    # print(len(df_above30))
    # df_above30
    # df_tmp

    # -------------------------------------------------------------------------------------------
    # Create masks to filter for

    # Create a boolean mask for temperatures above the threshold
    above_threshold_mask = df_tmp["voi"] > threshold_temperature

    # The current code turns the mask into a string. So, taking the boolean or cumsum array (if events that are 10 days or longer) leads to a longer string than the time series has. To remove events shorter than the selected threshold, we need a string of 0s and 1s only.
    above_threshold_mask10 = above_threshold_mask.astype(int)

    # Create dataframe that counts cumulative TRUES in the threshold mask
    consecutive_segments = (
        above_threshold_mask.astype(int)
        .groupby((~above_threshold_mask).cumsum())
        .cumsum()
    )

    # Extract the count of heatwaves
    hw_counts = (consecutive_segments == threshold_days).sum()

    # -------------------------------------------------------------------------------------------
    # Algorithm to remove days that are not part of a heatwave from the consecutive_segments data

    # Function turns following sequences to zero:
    # - 0{1 * threshold_days - 1}0
    # - ^{1 * threshold_days - 1}0
    # - 0{1 * threshold_days - 1}$
    # Example for 3 days threshold: 010, 0110, ^110, ^10, 011$, 01$
    str_sequence_org = "".join(above_threshold_mask10.astype(str))
    str_sequence = str_sequence_org

    # Create all in-between patterns
    mid_patterns = []
    start_patterns = []
    end_patterns = []

    n = threshold_days

    for i in range(n):
        # pat   = ''.join(np.arange(1, n-i).astype(str))
        pat = (n - i - 1) * "1"
        start = [pat + "0"]
        mid = ["0" + pat + "0"]
        end = ["0" + pat]

        mid_patterns = mid_patterns + mid
        start_patterns = start_patterns + start
        end_patterns = end_patterns + end

    # print(f'mid_patterns: {mid_patterns}')
    # print(f'end_patterns: {end_patterns}')
    # print(f'start_patterns: {start_patterns}')

    # Remove all occurrences of patterns from str_sequence
    for pattern in end_patterns:
        replacement = "0" * len(pattern)
        regex_pattern = re.compile(f"{pattern}$")
        str_sequence = regex_pattern.sub(replacement, str_sequence)

    for pattern in start_patterns:
        replacement = "0" * len(pattern)
        regex_pattern = re.compile(f"^{pattern}")
        str_sequence = regex_pattern.sub(replacement, str_sequence)

    # For some weird reason, we have to apply mid_pattern removal twice because first run does not capture sections like 0101.
    for pattern in mid_patterns:
        replacement = "0" * len(pattern)
        regex_pattern = re.compile(f"{pattern}")
        str_sequence = regex_pattern.sub(replacement, str_sequence)

    for pattern in mid_patterns:
        replacement = "0" * len(pattern)
        regex_pattern = re.compile(f"{pattern}")
        str_sequence = regex_pattern.sub(replacement, str_sequence)

    for pattern in mid_patterns:
        replacement = "0" * len(pattern)
        regex_pattern = re.compile(f"{pattern}")
        str_sequence = regex_pattern.sub(replacement, str_sequence)

    # print(str_sequence_org[990:1005])
    # print(str_sequence[990:1005])

    # Turn string into array again and update other arrays
    above_threshold_mask10_clean = np.fromiter(str_sequence, dtype=int)

    above_threshold_mask_clean = above_threshold_mask.astype(int)
    above_threshold_mask_clean[above_threshold_mask10_clean == 0] = 0

    consecutive_segments_clean = consecutive_segments.astype(int)
    consecutive_segments_clean[above_threshold_mask10_clean == 0] = 0

    # if verbose: print(consecutive_segments[1000:1005])
    # if verbose: print(consecutive_segments_clean[1000:1005])

    # if verbose: print(consecutive_segments[1660:1665])
    # if verbose: print(consecutive_segments_clean[1660:1665])

    # print(str_sequence)
    # print(np.array(consecutive_segments))
    # print(result_array)

    # print(above_threshold_mask10_clean)

    # -------------------------------------------------------------------------------------------
    # Extract metrics

    if hw_counts > 0:
        # Extract the longest duration of the heatwave
        hw_dur_max = consecutive_segments_clean.max()

        # Extract the sum of heatwave days
        hw_day_sum = above_threshold_mask_clean.sum()

        # Extract the mean duration of the heatwaves
        # IDEA: To get the median of heatwave duration, I would have to count the number of days for each heatwave and then add them together. But this requires more work and is not feasible right now.
        hw_dur_mean = hw_day_sum / hw_counts

        # Extract smallest duration between two events
        hw_days_between = extract_min_days_between_extremes(
            consecutive_segments_clean, df_tmp, threshold_days
        )

        # Extract mean and max temperature across all heatwaves
        df_temp = df_tmp[above_threshold_mask10_clean == 1].voi
        hw_mean_temp = df_temp.mean()
        hw_max_temp = df_temp.max()

    else:
        hw_dur_max = np.nan
        hw_dur_mean = np.nan
        hw_day_sum = np.nan
        hw_days_between = np.nan
        hw_mean_temp = np.nan
        hw_max_temp = np.nan

    # -------------------------------------------------------------------------------------------
    # Return metrics
    out = pd.DataFrame(
        {
            "hw_counts": hw_counts,
            "hw_dur_max": hw_dur_max,
            "hw_dur_mean": hw_dur_mean,
            "hw_day_sum": hw_day_sum,
            "hw_days_between": hw_days_between,
            "hw_mean_temp": hw_mean_temp,
            "hw_max_temp": hw_max_temp,
        },
        index=[0],
    )

    # Print results
    if verbose:
        print(f"hw_counts \t= {hw_counts}")
    if verbose:
        print(f"hw_dur_max \t= {hw_dur_max}")
    if verbose:
        print(f"hw_dur_mean \t= {hw_dur_mean}")
    if verbose:
        print(f"hw_day_sum \t= {hw_day_sum}")
    if verbose:
        print(f"hw_days_between = {hw_days_between}")
    if verbose:
        print(f"hw_mean_temp\t= {hw_mean_temp}")
    if verbose:
        print(f"hw_max_temp \t= {hw_max_temp }")

    # Debug return
    # if verbose:
    #     out = pd.concat(
    #         [consecutive_segments,
    #         consecutive_segments_clean,
    #         pd.Series(above_threshold_mask10),
    #         pd.Series(above_threshold_mask10_clean),
    #         df_tmp['voi'],
    #         df_tmp['date']],
    #         keys=['org_cumsum',
    #             'clean_cumsum',
    #             'org_01_mask',
    #             'clean_01_mask',
    #             'temperature_cols',
    #             'date'],
    #         axis=1)

    return out

# ------------------------------------------------------------------------------
def detect_frost_events(df_in, verbose = False):
    
    '''
    Function to detect frost events in a given dataframe.
    
    TODO: Add option to specify input variables.

    Returns:
        dataframe: with two variables. The first is the number of growing degree days before the last frost in spring. The second is the day of the first frost in fall.
    '''
    
    import numpy as np
    import pandas as pd
    
    # ----------------------------------------------------------------------------------------
    # Inputs
    first_year = df_in["first_year"].unique()[0]  # First year of data
    
    threshold_gdd = 5  # Temperature threshold to be counted as GDD
    threshold_fe = 0  # Temperature threshold for frost event

    variable_gdd = "mean_2m_air_temperature"  # Variable to use for assessing GDD
    variable_fe = (
        "minimum_2m_air_temperature"  # Variable to use for assessing frost event
    )

    ld_spring = "-05-31"  # Last day of spring
    ld_fall = "-11-30"  # Last day of fall

    # turn date into datetime object
    df_in["date"] = pd.to_datetime(
        df_in["date"]
    )  # Convert the 'date' column to datetime format

    # Loop over years of interest
    yoi = pd.Series(range(first_year, first_year + 5))
    vec_spring_events = pd.Series([np.nan], dtype="float64")
    vec_fall_events = pd.Series([np.nan], dtype="float64")

    for my_year in yoi:
        # ----------------------------------------------------------------------------------------
        # Frost Events
        # Set years of interest and date variables

        first_day = str(my_year) + "-01-01"
        last_day = str(my_year) + ld_spring

        first_day = pd.to_datetime(first_day)
        last_day = pd.to_datetime(last_day)

        # Keep only data on the spring
        df_spring = df_in.query("date >= @first_day and date <= @last_day")

        # Find the index of the last occurrence where mean_temp was below threshold
        df_filt = df_spring[df_spring[variable_fe] < threshold_fe]

        # Add check that if filtered df is empty, nan should be returned
        if df_filt.shape[0] == 0:
            gdd_count = np.nan

        else:
            last_below_zero_index = df_spring[
                df_spring[variable_fe] < threshold_fe
            ].index[-1]

            # Keep only data from the last occurrence and onwards
            df_spring = df_spring.loc[:last_below_zero_index]

            # Count gdd
            gdd_count = df_spring[df_spring[variable_gdd] > threshold_gdd].shape[0]

        if verbose:
            print(
                f"Number of growing degree days before the last frost in spring of {my_year}: {gdd_count}"
            )

        vec_spring_events = pd.concat([vec_spring_events, pd.Series(gdd_count)])

        # ----------------------------------------------------------------------------------------
        # Fall events
        first_day = str(my_year) + "-08-01"
        last_day = str(my_year) + ld_fall

        first_day = pd.to_datetime(first_day)
        last_day = pd.to_datetime(last_day)

        # Keep only data on the fall
        df_fall = df_in.query("date >= @first_day and date <= @last_day")

        # Find the index of the last occurrence where mean_temp was below threshold
        df_filt = df_fall[df_fall[variable_fe] < threshold_fe]

        # Add check that if filtered df is empty, nan should be returned
        if df_filt.shape[0] == 0:
            date_first_frost = np.nan
            doy_first_frost = np.nan

        else:
            date_first_frost = df_filt["date"].iloc[0]
            doy_first_frost = pd.to_datetime(date_first_frost).timetuple().tm_yday

        if verbose:
            print(
                f"The doy of the first frost in {my_year} is: {doy_first_frost} on the {date_first_frost}. \n"
            )

        vec_fall_events = pd.concat([vec_fall_events, pd.Series(doy_first_frost)])

    # For spring events, take largest GDD value because it is most destructive
    # For fall events, take earliest DOY value because it is most destructive

    out = pd.DataFrame(
        {
            "max_gdd_before_spring_frost": vec_spring_events.max(),
            "min_doy_of_fall_frost": vec_fall_events.min(),
        },
        index=[0],
    )

    return out

# ------------------------------------------------------------------------------

def perform_wrangling_on_all_sites(my_group, verbose = False, debug = False):
    
    import pandas as pd

    # Fix inputs
    df_tmp = my_group.copy()

    # Create empty dataframe to store results
    this_year = df_tmp["first_year"].unique()[0]
    this_id = df_tmp["SiteID"].unique()[0]
    
    df_out = pd.DataFrame({
        'SiteID': this_id,
        'first_year': this_year,
    }, index=[0])

    # Fix and attach variables
    if verbose: print("\n > Running function: fix_and_attach_variables()...")
    df_tmp = fix_and_attach_variables(df_tmp)
    # return df_tmp

    # Attach general temporal aggregates
    if verbose: print("\n > Running function: get_seasonal_aggregates()...")
        
    df_seas = get_seasonal_aggregates(
        df_in=df_tmp,
        timescale_days_to_months="fall cut-off",
        fcts_to_apply=["mean", "std"],
        debug=debug,
        verbose=False,
    )
    # return df_seas

    # Get heat wave information
    if verbose: print("\n > Running function: extract_heatwave_metrics()...")
    df_hw = extract_heatwave_metrics(
        df_in=df_tmp,
        threshold_temperature=30,
        threshold_days=3,
        variable_of_interest="mean_2m_air_temperature",
        verbose=False,
    )
    # return df_hw

    # Get frost event information
    if verbose: print("\n > Running function: detect_frost_events()...")
    df_fe = detect_frost_events(
        df_in=df_tmp,
        verbose=False)
    # return df_fe

    df_out = pd.concat([df_out, df_fe, df_hw, df_seas], axis=1)
    return df_out