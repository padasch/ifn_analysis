from sys import displayhook
import pandas as pd
import numpy as np
import geopandas as gpd
import os
import glob
from datetime import datetime
from datetime import datetime, date
from pyprojroot.here import here
import warnings
import matplotlib.pyplot as plt

import xarray as xr


from sys import displayhook
from sklearn import dummy
from tqdm import tqdm
import rasterio
import os
import shutil
import geopandas as gpd


def GENERAL_FUNCTIONS_____________________________________():
    pass


# -----------------------------------------------------------------------------
def load_hexmap():
    """
    Loads the hexmap from the data folder.
    """

    # Load hexmap
    hexmap = gpd.read_file(here("data/raw/maps/france_geojson/hex.geojson"))
    # Check if fresh hexmap is loaded (has not hex id yet)
    if "hex" in hexmap.columns:
        return hexmap

    # If hex id is not present, add and save hexmap
    hexmap = hexmap.reset_index()[["index", "geometry"]].rename(
        {"index": "hex"}, axis=1
    )
    hexmap.to_file(here("data/raw/maps/france_geojson/hex.geojson"), driver="GeoJSON")
    return hexmap


# -----------------------------------------------------------------------------
def filter_report(filter, df_before, df_after, site_level=False):
    if site_level:
        sites_before = df_before["idp"].nunique()
        sites_after = df_after["idp"].nunique()

        sites_removed = sites_before - sites_after
        sites_removed_percentage = round(100 - (sites_after / sites_before) * 100)

        print(
            f" - Filter: {filter:<30} |\tSites from {sites_before} to {sites_after} (= {sites_removed:>10}, {sites_removed_percentage}%)\t|\t",
            f"❗More than 5% of sites removed❗" if sites_removed_percentage > 5 else "",
        )

    else:
        trees_before = df_before["tree_id"].nunique()
        sites_before = df_before["idp"].nunique()

        trees_after = df_after["tree_id"].nunique()
        sites_after = df_after["idp"].nunique()

        sites_removed = sites_before - sites_after
        sites_removed_percentage = round(100 - (sites_after / sites_before) * 100)

        trees_removed = trees_before - trees_after
        trees_removed_percentage = round(100 - (trees_after / trees_before) * 100)

        print(
            f" - Filter: {filter:<30} |\tSites from {sites_before} to {sites_after} (= {sites_removed:>10}, {sites_removed_percentage}%)\t|\t",
            f"Trees from {trees_before} to {trees_after} (= {trees_removed:>5}, {trees_removed_percentage}%)",
            f"❗More than 5% of sites removed❗" if sites_removed_percentage > 5 else "",
            f"❗More than 5% of trees removed❗" if trees_removed_percentage > 5 else "",
        )


def NFI_WRANGLING_FUNCTIONS_____________________________________():
    pass


def get_latest_nfi_raw_data():
    # Print working directory

    # files = glob.glob(f"{here('data/tmp')}/*nfi_dataset_for_analysis*.csv")
    files = glob.glob(f"{here('data/tmp')}/*_nfi_dataset_raw.csv")

    # Sorting files by date so that the first in list is the latest
    files.sort(reverse=True)
    # Pick latest file
    latest_file = files[0]
    # Get the modification date and time of the latest file
    modification_time = os.path.getctime(latest_file)
    modification_date_time = datetime.fromtimestamp(modification_time).strftime(
        "%A %Y-%m-%d, %H:%M"
    )

    # Calculate the difference between today and the modification date
    today = date.today()
    modification_date = datetime.strptime(
        modification_date_time, "%A %Y-%m-%d, %H:%M"
    ).date()
    diff = today - modification_date

    print(
        f"👉 Latest file is {latest_file}",
        f"\n👉 Created on {modification_date_time} which is {diff.days} days ago.",
    )

    # Load the file
    nfi_data_raw = pd.read_csv(latest_file, index_col=0)
    return nfi_data_raw


def get_feature_database_sheet(sheet=None):
    if sheet is None:
        # Get sheets in the excel file
        sheets = pd.read_excel(
            here("docs/ifna_predictor_database.xlsx"),
            sheet_name=None,
        ).keys()

        print(f"Available sheets in the excel file:")
        for sheet in sheets:
            print(f"  - {sheet}")

        raise ValueError("Please provide a sheet name.")

    nfi_org = pd.read_excel(
        here("docs/ifna_predictor_database.xlsx"),
        sheet_name=sheet,
    )[["var", "type", "level", "remove"]]

    # Add suffixes _1 and _2 to the original variables to distinguish them if sampled from different years
    suffix_1 = nfi_org.copy()
    suffix_2 = nfi_org.copy()

    suffix_1["var"] = suffix_1["var"].astype(str).apply(lambda x: x + "_1")
    suffix_2["var"] = suffix_2["var"].astype(str).apply(lambda x: x + "_2")

    nfi_org_with_suffixes = pd.concat([nfi_org, suffix_1, suffix_2])
    return nfi_org_with_suffixes


# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
def calculate_growth_mortality(
    df_in,
    verbose=False,
    divide_by_nplots=False,
    grouping_variable="idp",
    per_year=True,
    min_trees_per_plot=None,
):
    """
    Function to calculate growth and mortality metrics for a given group.

    Raises:
        ValueError: _description_

    Returns:
        panda dataframe: mortality metrics per group
    """

    # Check if grouping variable is valid
    if grouping_variable not in df_in.columns:
        raise ValueError(
            f"Grouping variable {grouping_variable} is not in the dataframe."
        )

    # Get grouping variable
    my_group = df_in[grouping_variable].unique()[0]

    if verbose:
        print(
            f"> calculate_growth_mortality():\n  Calculating growth and mortality for {grouping_variable}: {my_group}."
        )

    # Check by how many years metrics of change must be divided
    if per_year:
        timespan = 5  # Default. This gives the change per year.
    else:
        timespan = 1  # This equals the entire change over 5 years.

    # -------------------------------------------------------------------------
    # Stem-based metrics
    # ! Important: To calculate natural stem-based mortality, we exclude trees that have been cut
    n_ini = df_in.query("tree_state_1 == 'alive' and tree_state_2 != 'cut'").shape[0]
    n_ini_withcuts = df_in.query("tree_state_1 == 'alive'").shape[0]
    n_sur = df_in.query("tree_state_change == 'alive_alive'").shape[0]
    n_rec = df_in.query("tree_state_change == 'new_alive'").shape[0]
    n_die = df_in.query("tree_state_change == 'alive_dead'").shape[0]
    n_cut = df_in.query("tree_state_change == 'alive_cut'").shape[0]
    n_fin = n_ini + n_rec

    # Check if metrics should be calculated based on min_trees_per_plot
    if min_trees_per_plot is not None:
        if n_ini < min_trees_per_plot:
            df_out = pd.DataFrame(
                {
                    grouping_variable: my_group,
                    "n_plots": np.nan,
                    # Tree count
                    "n_ini": np.nan,
                    "n_ini_withcuts": np.nan,
                    "n_sur": np.nan,
                    "n_fin": np.nan,
                    "n_rec": np.nan,
                    "n_die": np.nan,
                    "n_cut": np.nan,
                    # Basal Area
                    "ba_at_v1_of_alive_trees": np.nan,
                    "ba_at_v2_of_alive_trees": np.nan,
                    "ba_at_v1_of_survived": np.nan,
                    "ba_at_v2_of_survived": np.nan,
                    "ba_at_v1_of_died": np.nan,
                    "ba_at_v2_of_died": np.nan,
                    "ba_at_v2_of_recruited": np.nan,
                    "ba_at_v2_of_cut": np.nan,
                    # Change
                    # - Mortality
                    "mort_stems_prc_yr_esq": np.nan,
                    "mort_stems_prc_yr_hoshino": np.nan,
                    "mort_ba_prc_yr_hoshino": np.nan,
                    "mort_ba_yr_v1": np.nan,
                    "mort_ba_yr_v2": np.nan,
                    "mort_ba_prc_yr_v1": np.nan,
                    "mort_ba_prc_yr_v2": np.nan,
                    # - Growth
                    "rec_stems_prc_yr_hoshino": np.nan,
                    "tot_growth_ba_prc_yr_hoshino": np.nan,
                    "sur_growth_ba_prc_yr_hoshino": np.nan,
                    "tot_growth_ba_yr": np.nan,
                    "sur_growth_ba_yr": np.nan,
                    "tot_growth_ba_prc_yr": np.nan,
                    "sur_growth_ba_prc_yr": np.nan,
                    # - Cutting
                    "cut_ba_yr_v1": np.nan,
                    "cut_ba_prc_yr_v1": np.nan,
                },
                index=[0],
            )

            return df_out

    # -------------------------------------------------------------------------
    # Basal area-based metrics
    ba_at_v1_of_alive_trees = df_in[df_in["tree_state_1"] == "alive"]["ba_1"].sum()
    ba_at_v2_of_alive_trees = df_in.query(
        "tree_state_change == 'alive_alive' | tree_state_change == 'new_alive'"
    )["ba_2"].sum()

    ba_at_v1_of_survived = df_in.query("tree_state_change == 'alive_alive'")[
        "ba_1"
    ].sum()
    ba_at_v2_of_survived = df_in.query("tree_state_change == 'alive_alive'")[
        "ba_2"
    ].sum()

    ba_at_v1_of_died = df_in.query("tree_state_change == 'alive_dead'")["ba_1"].sum()
    ba_at_v2_of_died = df_in.query("tree_state_change == 'alive_dead'")["ba_2"].sum()
    ba_at_v2_of_recruited = df_in.query("tree_state_change == 'new_alive'")[
        "ba_2"
    ].sum()

    ba_at_v1_of_cut = df_in.query("tree_state_change == 'alive_cut'")["ba_1"].sum()

    # Mortality following Esquivel et al.
    if n_ini == 0:
        mort_stems_prc_yr_esq = np.nan
    else:
        mort_stems_prc_yr_esq = (1 - (n_sur / n_ini) ** (1 / timespan)) * 100

    # Mortality and Recruitment following Hoshino et al.
    if n_sur == 0:
        mort_stems_prc_yr_hoshino = np.nan
        rec_stems_prc_yr_hoshino = np.nan
    else:
        mort_stems_prc_yr_hoshino = np.log(n_ini / n_sur) / timespan * 100
        rec_stems_prc_yr_hoshino = np.log(n_fin / n_sur) / timespan * 100

    # Loss, gain, ingrowth following Hoshino et al.
    if ba_at_v1_of_survived == 0:
        mort_ba_prc_yr_hoshino = np.nan
        tot_growth_ba_prc_yr_hoshino = np.nan
        sur_growth_ba_prc_yr_hoshino = np.nan
    else:
        mort_ba_prc_yr_hoshino = (
            np.log(ba_at_v1_of_alive_trees / ba_at_v1_of_survived) / timespan * 100
        )
        tot_growth_ba_prc_yr_hoshino = (
            np.log(ba_at_v2_of_alive_trees / ba_at_v1_of_survived) / timespan * 100
        )
        sur_growth_ba_prc_yr_hoshino = (
            np.log(ba_at_v2_of_survived / ba_at_v1_of_survived) / timespan * 100
        )

    # Absolute changes
    tot_growth_ba_yr = (ba_at_v2_of_alive_trees - ba_at_v1_of_survived) / timespan
    sur_growth_ba_yr = (ba_at_v2_of_survived - ba_at_v1_of_survived) / timespan
    mort_ba_yr_v1 = ba_at_v1_of_died / timespan
    mort_ba_yr_v2 = ba_at_v2_of_died / timespan
    cut_ba_yr_v1 = ba_at_v1_of_cut / timespan

    # Relative changes
    #  - Growth
    if ba_at_v1_of_survived == 0:
        tot_growth_ba_prc_yr = np.nan
        sur_growth_ba_prc_yr = np.nan
    else:
        tot_growth_ba_prc_yr = tot_growth_ba_yr / ba_at_v1_of_survived * 100
        sur_growth_ba_prc_yr = sur_growth_ba_yr / ba_at_v1_of_survived * 100

    # - Mortality
    #   - With respect to total basal area at v1
    if ba_at_v1_of_alive_trees == 0:
        mort_ba_prc_yr_v1 = np.nan
    else:
        mort_ba_prc_yr_v1 = mort_ba_yr_v1 / ba_at_v1_of_alive_trees * 100

    #   - With respect to total basal area at v2 (not very logical...)
    if (ba_at_v2_of_died + ba_at_v2_of_alive_trees) == 0:
        mort_ba_prc_yr_v2 = np.nan
    else:
        mort_ba_prc_yr_v2 = (
            mort_ba_yr_v2 / (ba_at_v2_of_died + ba_at_v2_of_alive_trees) * 100
        )

    # - Cutting
    if ba_at_v1_of_alive_trees == 0:
        cut_ba_prc_yr_v1 = np.nan
    else:
        cut_ba_prc_yr_v1 = cut_ba_yr_v1 / ba_at_v1_of_alive_trees * 100

    # -------------------------------------------------------------------------
    # Divide by number of plots if required
    if divide_by_nplots:
        if verbose:
            print(
                f"\n Dividing all metrics by number of plots per grouping variable: {grouping_variable} so that they are in terms of per plot."
            )

        raise ValueError(
            "This is not implemented yet. Can be simplified by just dividing final df by n_plots, except for grouping variable."
        )

        if (divide_by_nplots) & (grouping_variable == "idp"):
            os.error("Cannot divide by number of plots if grouping variable is idp.")

        # n_plots = df_in["idp"].nunique()
        # # Tree counts
        # n_ini = n_ini / n_plots
        # n_sur = n_sur / n_plots
        # n_fin = n_fin / n_plots
        # n_rec = n_rec / n_plots
        # n_die = n_die / n_plots
        # n_cut = n_cut / n_plots
        # # Basal Area
        # ba_at_v1_of_alive_trees = ba_at_v1_of_alive_trees / n_plots
        # ba_at_v2_of_alive_trees = ba_at_v2_of_alive_trees / n_plots
        # ba_at_v1_of_survived = ba_at_v1_of_survived / n_plots
        # ba_at_v2_of_survived = ba_at_v2_of_survived / n_plots
        # ba_at_v2_of_died = ba_at_v2_of_died / n_plots
        # ba_at_v2_of_recruited = ba_at_v2_of_recruited / n_plots
        # # Changes
        # # - Mortality
        # mort_stems_prc_yr_esq = mort_stems_prc_yr_esq / n_plots
        # mort_stems_prc_yr_hoshino = mort_stems_prc_yr_hoshino / n_plots
        # mort_ba_prc_yr_hoshino = mort_ba_prc_yr_hoshino / n_plots
        # mort_ba_yr_v1 = mort_ba_yr_v1 / n_plots
        # mort_ba_yr_v2 = mort_ba_yr_v2 / n_plots
        # mort_ba_prc_yr_v1 = mort_ba_prc_yr_v1 / n_plots
        # mort_ba_prc_yr_v2 = mort_ba_prc_yr_v2 / n_plots
        # # - Growth
        # rec_stems_prc_yr_hoshino = rec_stems_prc_yr_hoshino / n_plots
        # tot_growth_ba_prc_yr_hoshino = tot_growth_ba_prc_yr_hoshino / n_plots
        # tot_growth_ba_yr = tot_growth_ba_yr / n_plots
        # tot_growth_ba_prc_yr = tot_growth_ba_prc_yr / n_plots
        # sur_growth_ba_prc_yr_hoshino = sur_growth_ba_prc_yr_hoshino / n_plots
        # sur_growth_ba_yr = sur_growth_ba_yr / n_plots
        # sur_growth_ba_prc_yr = sur_growth_ba_prc_yr / n_plots
        # # - Cutting

    else:
        n_plots = df_in["idp"].nunique()

    df_out = pd.DataFrame(
        {
            grouping_variable: my_group,
            "n_plots": n_plots,
            # Tree count
            "n_ini": n_ini,
            "n_ini_withcuts": n_ini_withcuts,
            "n_sur": n_sur,
            "n_fin": n_fin,
            "n_rec": n_rec,
            "n_die": n_die,
            "n_cut": n_cut,
            # Basal Area
            "ba_at_v1_of_alive_trees": ba_at_v1_of_alive_trees,
            "ba_at_v2_of_alive_trees": ba_at_v2_of_alive_trees,
            "ba_at_v1_of_survived": ba_at_v1_of_survived,
            "ba_at_v2_of_survived": ba_at_v2_of_survived,
            "ba_at_v1_of_died": ba_at_v1_of_died,
            "ba_at_v2_of_died": ba_at_v2_of_died,
            "ba_at_v2_of_recruited": ba_at_v2_of_recruited,
            "ba_at_v2_of_cut": ba_at_v1_of_cut,
            # Change
            # - Mortality
            "mort_stems_prc_yr_esq": mort_stems_prc_yr_esq,
            "mort_stems_prc_yr_hoshino": mort_stems_prc_yr_hoshino,
            "mort_ba_prc_yr_hoshino": mort_ba_prc_yr_hoshino,
            "mort_ba_yr_v1": mort_ba_yr_v1,
            "mort_ba_yr_v2": mort_ba_yr_v2,
            "mort_ba_prc_yr_v1": mort_ba_prc_yr_v1,
            "mort_ba_prc_yr_v2": mort_ba_prc_yr_v2,
            # - Growth
            "rec_stems_prc_yr_hoshino": rec_stems_prc_yr_hoshino,
            "tot_growth_ba_prc_yr_hoshino": tot_growth_ba_prc_yr_hoshino,
            "sur_growth_ba_prc_yr_hoshino": sur_growth_ba_prc_yr_hoshino,
            "tot_growth_ba_yr": tot_growth_ba_yr,
            "sur_growth_ba_yr": sur_growth_ba_yr,
            "tot_growth_ba_prc_yr": tot_growth_ba_prc_yr,
            "sur_growth_ba_prc_yr": sur_growth_ba_prc_yr,
            # - Cutting
            "cut_ba_yr_v1": cut_ba_yr_v1,
            "cut_ba_prc_yr_v1": cut_ba_prc_yr_v1,
        },
        index=[0],
    )

    return df_out


# -----------------------------------------------------------------------------
def extract_nspecies(df_in, species_vars=["espar_red", "species_lat", "genus_lat"]):
    df_out = pd.DataFrame({"group_id": df_in["group_id"].unique()})
    for var in species_vars:
        df_out[f"n_species_{var}"] = df_in[var].nunique()

    return df_out


# extract_nspecies(df_list[125], ["espar_red", "species_lat", "genus_lat"])
# -----------------------------------------------------------------------------
def extract_statistics_general(df_in, vars_in, fcts_to_apply):
    """
    This function extracts the desired statistics from the input dataframe.

    Returns:
        _type_: _description_
    """

    df_out = pd.DataFrame({"group_id": df_in["group_id"].unique()})

    fct_dict = {
        "mean": np.nanmean,
        "std": np.nanstd,
        "median": np.nanmedian,
        "max": np.nanmax,
        "min": np.nanmin,
        "range": range_func,
        "sum": np.nansum,
        "iqr": iqr_func,
    }

    # Loop through functions
    for my_fct in fcts_to_apply:
        # print(my_fct)
        # Loop through variables
        for var_in in vars_in:
            # print(var_in)
            var_name = var_in + "_" + my_fct

            # If array is empty or if all values are NA, set to NA
            if (len(df_in[var_in]) == 0) or (df_in[var_in].isna().all()):
                my_value = np.nan
            else:
                my_value = fct_dict[my_fct](df_in[var_in])

            # print(my_value)
            df_out[var_name] = my_value

    return df_out


# xxx = extract_statistics_general(
#     df_in=df_list[125],
#     vars_in=["htot", "age13", "ir5", "v", "ba_1", "ba_2"],
#     fcts_to_apply=["mean", "std", "median", "max", "min", "sum", "range", "iqr"],
# ).T


# for i in range(xxx.shape[0]):
#     print(f"{xxx.index[i]:<30} = {xxx.iloc[i].values}")
# -----------------------------------------------------------------------------
def extract_statistics_per_visit_and_tree_state(df_in):
    """
    This function extracts statistics per tree state (alive, dead, rec) for each visit. These metrics provide information on the number and basal area of these tree states in relation to the total area or number of trees in a plot.

    Raises:
        ValueError: _description_

    Returns:
        _type_: _description_
    """

    df_out = pd.DataFrame({"group_id": df_in["group_id"].unique()})

    for i_visit in [1, 2]:
        var_ba = f"ba_{i_visit}"
        var_vis = f"v{i_visit}"
        var_state = f"tree_state_{i_visit}"

        # Remove recruits because they are technically not there yet
        # This only applies to the first visit group
        i_df = df_in.query(f"{var_state} != 'new'")

        groups = i_df.groupby(var_state, as_index=False)

        df_out[f"static_full_stand_ba_at_{var_vis}"] = i_df[var_ba].sum()
        df_out[f"static_full_stand_nt_at_{var_vis}"] = i_df.shape[0]

        # * When adding new variables, make sure to add them to the if and else statement sections below!
        for gname, group in groups:
            # Skip group for recruits
            if gname == "new":
                continue

            if group.shape[0] > 0:
                # Total Area and Ntrees per group
                df_out[f"static_{gname}_ba_at_{var_vis}"] = group[var_ba].sum()
                df_out[f"static_{gname}_nt_at_{var_vis}"] = group.shape[0]

                # Percentage of total area and Number of trees per group
                if df_out[f"static_full_stand_ba_at_{var_vis}"].iloc[0] == 0:
                    df_out[f"static_{gname}_ba_at_{var_vis}_perc"] = 0
                else:
                    df_out[f"static_{gname}_ba_at_{var_vis}_perc"] = (
                        group[var_ba].sum()
                        / df_out[f"static_full_stand_ba_at_{var_vis}"].iloc[0]
                    )

                if df_out[f"static_full_stand_nt_at_{var_vis}"].iloc[0] == 0:
                    df_out[f"static_{gname}_nt_at_{var_vis}_perc"] = 0
                else:
                    df_out[f"static_{gname}_nt_at_{var_vis}_perc"] = (
                        group.shape[0]
                        / df_out[f"static_full_stand_nt_at_{var_vis}"].iloc[0]
                    )

            else:
                # Return zeros
                df_out[f"static_{gname}_ba_at_{var_vis}"] = 0
                df_out[f"static_{gname}_nt_at_{var_vis}"] = 0
                df_out[f"static_{gname}_ba_at_{var_vis}_perc"] = 0
                df_out[f"static_{gname}_nt_at_{var_vis}_perc"] = 0

    # # Quality Control
    # espected_cols = len(v1_groups) * 8 + 1  # group_id + 8 variables * number of groups
    # actual_cols = df_out.shape[1]
    # if actual_cols != espected_cols:
    #     raise ValueError(
    #         f"Expected output shape is not correct! Espected: {espected_cols} | Actual: {actual_cols}"
    #     )

    return df_out


# xxx = extract_statistics_per_visit_and_tree_state(df_list[125]).T
# for i in range(xxx.shape[0]):
#     print(f"{xxx.index[i]:<30} = {xxx.iloc[i].values}")
# -----------------------------------------------------------------------------§
def extract_stand_characterstics_for_all_and_topn_species(
    df_in,
    vars_in=["htot", "age13", "ir5", "v", "ba_1", "ba_2", "ba_change_perc_yr"],
    fcts_to_apply=["mean", "std", "median", "max", "min", "sum", "range", "iqr"],
    species_var="espar_red",
    n_groups=3,
):
    # Ignore certain warnings caused by empty dfs or NA slices
    warnings.filterwarnings("ignore", message="All-NaN")
    warnings.filterwarnings("ignore", message="Degrees")
    warnings.filterwarnings("ignore", message="Mean")

    # Get output df
    df_out = pd.DataFrame({"group_id": df_in["group_id"].unique()})

    # Extraction for all species ---------------------------------------------
    df_add = extract_nspecies(df_in).drop("group_id", axis=1)
    df_out = pd.concat([df_out, df_add], axis=1)
    df_add = extract_statistics_general(
        df_in=df_in,
        vars_in=vars_in,
        fcts_to_apply=fcts_to_apply,
    )

    # Attach prefix to column names
    df_add = df_add.add_prefix("allspecies_")
    df_add = df_add.drop(columns=["allspecies_group_id"])

    df_out = pd.concat([df_out, df_add], axis=1)

    # Extraction per top n species -------------------------------------------
    # Get sum of ba_1 per group

    top3species_groups = (
        (
            df_in.groupby(species_var)
            .agg({"ba_1": "sum"})
            .reset_index()
            .sort_values(by="ba_1", ascending=False)
        )
        .head(n_groups)[species_var]
        .tolist()
    )

    # Prepare empty df
    for i in range(n_groups):
        df_i = df_in.query(f"{species_var} == @top3species_groups[{i}]")

        # If df is empty, set species to None
        if df_i.shape[0] == 0:
            top3species_groups[i] = "None"

        # display(df_i)
        df_i = extract_statistics_general(
            df_in=df_i,
            vars_in=vars_in,
            fcts_to_apply=fcts_to_apply,
        )

        # Attach prefix to column names
        df_i = df_i.add_prefix(f"top{i+1}_")
        df_i[f"top{i+1}_{species_var}"] = str(top3species_groups[i])
        df_i = df_i.rename(columns={f"top{i+1}_group_id": "group_id"})
        df_i = df_i.drop(columns=["group_id"])

        df_out = pd.concat([df_out, df_i], axis=1)

        # df_out = df_out.merge(df_i, on="group_id", how="left")
        # display(df_out)

    # Reset warnings
    warnings.filterwarnings("default")

    return df_out


# xxx = extract_stand_characterstics_for_all_and_topn_species(
#     df_in=df_list[125],
#     vars_in=["htot", "age13", "ir5", "v", "ba_1"],
#     fcts_to_apply=["mean", "std", "median", "max", "min", "sum", "range", "iqr"],
#     species_var="genus_lat",
# ).T.sort_index()
# for i in range(xxx.shape[0]):
#     print(f"{xxx.index[i]:<25} = {xxx.iloc[i].values}")
# -----------------------------------------------------------------------------
def extract_tree_damage_info(df_in):
    """
    For all trees that are alive, assess the damage that they have at first and at second visit.
    """

    # Define output df
    df_out = pd.DataFrame({"group_id": df_in["group_id"].unique()})

    for tree_condition in ["alive", "died", "survived"]:
        # alive = looking at all trees that are alive at first visit or at second visit
        # died = looking at all trees that died between first and second visit
        # survived = looking at all trees that survived between first and second visit

        # Get subset of alive trees
        if tree_condition == "alive":
            # Here we need to separate because we want to include recruits in
            # the second visit site assessment.
            df_1 = df_in.query("tree_state_1 == 'alive'")
            df_2 = df_in.query("tree_state_2 == 'alive'")

        if tree_condition == "died":
            # Here we do not need to separate because we are focusing already on
            # trees that died between first and second visit.
            df_1 = df_in.query("tree_state_change == 'alive_dead'")
            df_2 = df_in.query("tree_state_change == 'alive_dead'")

        if tree_condition == "survived":
            # Here we do not need to separate because we are focusing already on
            # trees that survived between first and second visit.
            df_1 = df_in.query("tree_state_change == 'alive_alive'")
            df_2 = df_in.query("tree_state_change == 'alive_alive'")

        nt_1 = df_1.shape[0]
        nt_2 = df_2.shape[0]

        # Print for debug -------------------------------------------------------------
        # display(
        #     df_1[
        #         [
        #             "tree_state_change",
        #             "acci",
        #             "deggib",
        #             "sfpied",
        #             "sfcoeur",
        #             "sfgui_1",
        #             "sfgeliv_1",
        #             "sfdorge_1",
        #             "mortb_1",
        #             "mortb_2",
        #         ]
        #     ]
        # )

        # print(df_1.shape)
        # print(nt_1)
        # print(nt_2)

        # First visit -------------------------------------------------------------
        # Structural damange
        n_struct = df_1.query("acci in ['1', '2', '3']").shape[0]
        df_out[f"trees_{tree_condition}_with_structdmg_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_struct / nt_1
        )

        # Fire
        n_fire = df_1.query("acci == '4'").shape[0]
        df_out[f"trees_{tree_condition}_with_burn_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_fire / nt_1
        )

        # Game damage
        n_game = df_1.query("deggib not in ['0', 'Missing']").shape[0]
        df_out[f"trees_{tree_condition}_with_game_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_game / nt_1
        )

        # Foot damage
        n_foot = df_1.query("sfpied not in ['0', 'Missing']").shape[0]
        df_out[f"trees_{tree_condition}_with_foot_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_foot / nt_1
        )

        # Rot core
        n_rot = df_1.query("sfcoeur not in ['0', 'Missing']").shape[0]
        df_out[f"trees_{tree_condition}_with_rot_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_rot / nt_1
        )

        # Mistletoe (0 = none, 1 = 1 or two, 2 = 3 or 5, 3 = 6 or more)
        n_mist = df_1.query("sfgui_1 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_mistl_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_mist / nt_1
        )

        # Frost damage
        n_frost = df_1.query("sfgeliv_1 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_frost_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_frost / nt_1
        )

        # Fir rust (0 = none, 1 = 1 frost damage, 2 = 2 two or more)
        n_firrust = df_1.query("sfdorge_1 not in ['0', 'Missing']").shape[0]
        df_out[f"trees_{tree_condition}_with_firrust_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_firrust / nt_1
        )

        # Branch mortality
        n_mortb = df_1.query("mortb_1 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_branchdmg_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_mortb / nt_1
        )

        # Any damage
        n_any = df_1.query(
            "acci in ['1', '2', '3'] | acci == '4' | deggib not in ['0', 'Missing'] | sfpied not in ['0', 'Missing'] | sfcoeur not in ['0', 'Missing'] | sfgui_1 > 0 | sfgeliv_1 > 0 | sfdorge_1 not in ['0', 'Missing'] | mortb_1 > 0"
        ).shape[0]

        df_out[f"trees_{tree_condition}_with_anydmg_at_v1_in_perc"] = (
            np.nan if nt_1 == 0 else n_any / nt_1
        )

        # Second visit -----------------------------------------------------------
        # Frost damage
        n_frost = df_2.query("sfgeliv_2 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_frost_at_v2_in_perc"] = (
            np.nan if nt_2 == 0 else n_frost / nt_2
        )

        # Mistletoe (0 = none, 1 = 1 or two, 2 = 3 or 5, 3 = 6 or more)
        n_mist = df_2.query("sfgui_2 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_mistl_at_v2_in_perc"] = (
            np.nan if nt_2 == 0 else n_mist / nt_2
        )

        # Fir rust (0 = none, 1 = 1 frost damage, 2 = 2 two or more)
        n_firrust = df_2.query("sfdorge_2 not in ['0', 'Missing']").shape[0]
        df_out[f"trees_{tree_condition}_with_firrust_at_v2_in_perc"] = (
            np.nan if nt_2 == 0 else n_firrust / nt_2
        )

        # Branch mortality
        n_mortb = df_2.query("mortb_2 > 0").shape[0]
        df_out[f"trees_{tree_condition}_with_branchdmg_at_v2_in_perc"] = (
            np.nan if nt_2 == 0 else n_mortb / nt_2
        )

        # Any damage
        n_any = df_2.query(
            "sfgeliv_2 > 0 | sfgui_2 > 0 | sfdorge_2 not in ['0', 'Missing'] | mortb_2 > 0"
        ).shape[0]
        df_out[f"trees_{tree_condition}_with_anydmg_at_v2_in_perc"] = (
            np.nan if nt_2 == 0 else n_any / nt_2
        )

    # Check if any value except group_id is above 1.0
    if (sum(df_out[col] > 1.0 for col in df_out.columns if col != "group_id") != 0)[0]:
        raise ValueError(
            "Invalid value found in df_out. At least one value is above 1.0!"
        )

    return df_out


# extract_tree_damage_info(df_list[666])
# xxx = extract_tree_damage_info(df_in).T
# for i in range(xxx.shape[0]):
#     print(f"{xxx.index[i]:<40} = {xxx.iloc[i].values}")
# -----------------------------------------------------------------------------
def aggregate_tree_info_to_site_level(df_in, vars_in, fcts_to_apply, species_var):
    # Extract stand characteristics
    df_1 = extract_stand_characterstics_for_all_and_topn_species(
        df_in=df_in,
        vars_in=vars_in,
        fcts_to_apply=fcts_to_apply,
        species_var=species_var,
        n_groups=3,
    )

    # Extract stand statistics per visit
    df_2 = extract_statistics_per_visit_and_tree_state(df_in)
    # Extract tree damage info
    df_3 = extract_tree_damage_info(df_in)
    # Extract growth and mortality
    # TODO: Outcommenting this now. Not sure why I added this...
    # df_4 = calculate_growth_mortality(df_in).rename(columns={"idp": "group_id"})

    df_out = pd.concat(
        [
            df_1,
            df_2.drop("group_id", axis=1),
            df_3.drop("group_id", axis=1),
            # df_4.drop("group_id", axis=1),
        ],
        axis=1,
    )

    return df_out


# -----------------------------------------------------------------------------
def attach_regional_information(df_in, verbose=False):
    # Load shapefiles
    # * All geojson files are in CRS 2154, so using lat_fr and lon_fr for referencing!
    shp_reg = gpd.read_file(here("data/raw/maps/france_geojson/reg.geojson"))
    shp_dep = gpd.read_file(here("data/raw/maps/france_geojson/dep.geojson"))
    shp_gre = gpd.read_file(here("data/raw/maps/france_geojson/gre.geojson"))
    shp_ser = gpd.read_file(here("data/raw/maps/france_geojson/ser.geojson"))
    shp_hex = load_hexmap()

    # Add variables
    df_in["reg"] = "Missing"
    df_in["dep"] = "Missing"
    df_in["gre"] = "Missing"
    df_in["ser"] = "Missing"
    df_in["hex"] = "Missing"

    # Import Point function
    from shapely.geometry import Point

    # TODO: Ignore chained assignment warning. Not sure if this is the best way to do it...
    pd.options.mode.chained_assignment = None

    # Loop over every site and get the corresponding regional code
    for i in tqdm(range(df_in.shape[0]), disable=not verbose):
        # Get site coordinates
        lat = df_in["lat_fr"].iloc[i]
        lon = df_in["lon_fr"].iloc[i]

        # Add information from closest polygon to df
        df_in["reg"].iloc[i] = shp_reg.iloc[shp_reg.distance(Point(lon, lat)).idxmin()][
            "reg"
        ]
        df_in["dep"].iloc[i] = shp_dep.iloc[shp_dep.distance(Point(lon, lat)).idxmin()][
            "dep"
        ]
        df_in["gre"].iloc[i] = shp_gre.iloc[shp_gre.distance(Point(lon, lat)).idxmin()][
            "gre"
        ]
        df_in["ser"].iloc[i] = shp_ser.iloc[shp_ser.distance(Point(lon, lat)).idxmin()][
            "ser"
        ]
        df_in["hex"].iloc[i] = shp_hex.iloc[shp_hex.distance(Point(lon, lat)).idxmin()][
            "hex"
        ]

    return df_in


# -----------------------------------------------------------------------------
def split_df_into_list_of_group_or_ns(df_in, group_variable=10):
    if type(group_variable) == int:
        print(f"Splitting df into {group_variable} random groups")
        df_list = np.array_split(df_in, group_variable)

    else:
        # Check if group_variable is in df
        if group_variable not in df_in.columns:
            raise ValueError(f"Group variable {group_variable} not in df!")

        # Group by group_variable and turn into list
        df_list = [df for _, df in df_in.groupby(group_variable)]

    return df_list


# -----------------------------------------------------------------------------

# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------
# -----------------------------------------------------------------------------


def RASTER_WRANGLING_FUNCTIONS_____________________________________():
    pass


# ------------------------------------------------------------------------------------------


def extract_raster_values(
    tiff_file,
    variable_name,
    latitudes,
    longitudes,
    progress_bar=False,
    expected_crs=None,
):
    if expected_crs is None:
        print(
            f"\t🚧 WARNING: No CRS specified. Make sure inputed coordinates have matching CRS!."
        )
        print(f"\t Returning None!")
        return None

    # Open the TIFF file
    with rasterio.open(tiff_file) as src:
        # Print the CRS (Coordinate Reference System) for quality control
        # Get temporary CRS
        tmp_crs = str(src.crs)

        # Bugfix for Agroparistech data
        if "RGF_1993_Lambert_Conformal" in tmp_crs:
            tmp_crs = "EPSG:2154"

        if not expected_crs in tmp_crs:
            print(
                f"\t🚧 WARNING: The CRS is {tmp_crs} and not {expected_crs}. Make sure inputed coordinates have matching CRS!."
            )
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
        # Turn all elements in raster_values to float to avoid issues merging integer with float-NaN
        raster_values = [float(i) for i in raster_values]

        df = pd.DataFrame(
            {
                variable_name: raster_values,
                "Latitude": latitudes_out,
                "Longitude": longitudes_out,
            }
        )

        return df


# ------------------------------------------------------------------------------------------
def parallel_agroparistech_extraction(
    group_in, df_coords, progress_bar=False, verbose=True, concat_by_axis=1
):
    # Get df_all for column-wise merging
    df_all = df_coords[["idp", "x_fr", "y_fr"]]

    # Get df_row for row-wise merging
    df_row = pd.DataFrame()

    # Print working directory
    # print("Working directory: ", os.getcwd())

    for i in range(len(group_in)):
        # Print progress
        if verbose:
            print(
                f"\nGroup {group_in['group'].iloc[i]} \t | {i+1}/{len(group_in)} | {group_in['variables'].iloc[i]}.tif | {group_in['crs'].iloc[i]} | {group_in['files'].iloc[i]}",
                end="\t",
            )

        # Extract values from raster
        df = extract_raster_values(
            group_in["files"].iloc[i],
            group_in["variables"].iloc[i],
            df_coords["y_fr"],
            df_coords["x_fr"],
            progress_bar=progress_bar,
            expected_crs=group_in["crs"].iloc[i],
        )

        # Merge with outgoing df
        # Depends on whether we want to concatenate by row or column (row = slow files, column = fast files)

        if concat_by_axis == 0:
            # Fix coordinates naming to fit input
            df = df.rename(columns={"Latitude": "y_fr", "Longitude": "x_fr"})

            # Attach idp back again to avoid issues when merging
            df["idp"] = df_all.copy()["idp"]

            # Then drop all na values do avoid duplicating sites when row-wise concatenating
            # Plus, reset index just in case
            df = df.dropna().reset_index(drop=True)
            df_row = df_row.reset_index(drop=True)

            # Finally, concatenate by row
            df_row = pd.concat([df_row, df], axis=concat_by_axis)

        else:
            df = df.reset_index(drop=True)
            df_all = df_all.reset_index(drop=True)
            df_all = pd.concat(
                [df_all, df[group_in["variables"].iloc[i]]], axis=concat_by_axis
            )

    if concat_by_axis == 0:
        return df_row
    else:
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


def wrapper_for_large_files(
    group_in, tif_in, var_in, crs_in, progress_bar=False, verbose=False
):
    # Start verbose output
    if verbose:
        print(
            f"wrapper_for_large_files():\n - Working on group {group_in['group'].unique()[0]}..."
        )

    # Need to create a separate tif_in for each group_in
    # Else, parallelization does not work

    tif_in_tmp = tif_in + "_" + group_in["group"].unique()[0].astype(str)
    tif_in_tmp = tif_in
    if verbose:
        print(f" - Creating temporary tif: {tif_in_tmp}...")
    # shutil.copy(tif_in, tif_in_tmp)

    # Run extraction
    if verbose:
        print(" - Extracting values from raster...")
    df = extract_raster_values(
        tiff_file=tif_in_tmp,
        variable_name=var_in,
        latitudes=group_in["y"],
        longitudes=group_in["x"],
        progress_bar=progress_bar,
        expected_crs=crs_in,
    )

    # Merge extracted data
    df = df.drop(columns=["Latitude", "Longitude"]).reset_index(drop=True)
    df = pd.concat([group_in, df], axis=1)

    # Delete temporary tif
    if verbose:
        print(" - Deleting temporary tif...")
    # os.remove(tif_in_tmp)

    return df


# ------------------------------------------------------------------------------------------
# FUNCTIONS TO EXTRACT EDO DATA
# ------------------------------------------------------------------------------------------
# ------------------------------------------------------------------------------------------
def parallel_edo_extraction(
    group_in, df_sites, progress_bar=False, debug=False, expected_crs=None
):
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
def seasonal_aggregation_per_site(
    df_in, current_var, fcts_to_apply, progress_bar=False
):
    grouped = df_in.groupby("idp")  # Group by idp
    df_list = [group for name, group in grouped]  # Create list
    df_out = pd.DataFrame()  # Create empty dataframe for output

    for i in tqdm(range(len(df_list)), disable=not progress_bar):
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

    supported_fcts = ["mean", "std", "median", "max", "min", "range", "sum", "iqr"]
    default_fcts = ["mean", "std"]

    if fcts_to_apply is None:
        print(
            f"No functions provided! Using default: {default_fcts}.",
            f"Supported functions are: {supported_fcts}",
        )

    # Settings
    # timescale_days_to_months = "fall cut-off"
    # fcts_to_apply     = ['mean', 'std']
    # fcts_to_apply     = ['mean', 'std', 'median', 'max', 'min']

    first_year = df_in["first_year"].unique()[0]

    vars_to_aggregate = df_in.drop(
        columns=["date", "idp", "SiteID", "season", "first_year"],
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
    fct_dict = {
        "mean": np.nanmean,
        "std": np.nanstd,
        "median": np.nanmedian,
        "max": np.nanmax,
        "min": np.nanmin,
        "range": range_func,
        "sum": np.nansum,
        "iqr": iqr_func,
    }

    # Loop through functions
    for my_fct in fcts_to_apply:
        # print(my_fct)
        # Loop through variables
        for my_var in vars_to_aggregate:
            # print(my_var)
            # my_var = my_var[0]
            df_tmp = df_filtered_daterange.groupby("season", observed=False)[
                my_var
            ].agg(fct_dict[my_fct])

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
    x = x.dropna()
    return x.max() - x.min()


def iqr_func(x):
    x = x.dropna()
    return x.quantile(0.75) - x.quantile(0.25)


# ------------------------------------------------------------------------------------------
# FUNCTIONS FOR EXTRACTING EXTREME EVENTS FROM EDO DATA


def extract_extreme_events_per_idp(df_in):
    # Get df with idp
    df_befaft_all = pd.DataFrame({"idp": df_in["idp"].unique()})
    df_grouped_by_befaft = df_in.groupby("before_first_year")

    for _, g_befaft in df_grouped_by_befaft:
        # Get metrics on extreme events
        df_metrics_all = pd.DataFrame()
        # df_metrics_all = pd.DataFrame({"idp": g_befaft["idp"].unique()})

        # Loop over groups heat and cold waves
        df_grouped_by_wave = g_befaft.groupby("heat_or_cold")
        for _, g_wave in df_grouped_by_wave:
            # Extract metrics
            df_metrics = extract_extreme_wave_metrics(g_wave)
            # Merge metrics
            df_metrics_all = pd.concat([df_metrics_all, df_metrics], axis=1)

        # Add suffix based on before or after
        is_before = g_befaft["before_first_year"].unique()[0]
        # display(is_before)
        if is_before:
            df_metrics_all = df_metrics_all.add_suffix("_tmin5")
        else:
            df_metrics_all = df_metrics_all.add_suffix("_tpls5")

        # display(df_metrics_all)

        # Concatenate to df_befaft_all column-wise
        df_befaft_all = pd.concat([df_befaft_all, df_metrics_all], axis=1)

    return df_befaft_all


# ------------------------------------------------------------------------------------------
def extract_extreme_wave_metrics(group_in):
    # Input
    wave_type = group_in["heat_or_cold"].unique()[0]
    # display(wave_type)

    # Prepare empty dataframe
    df_out = pd.DataFrame(
        {
            "n_events": np.nan,
            "temp_min": np.nan,
            "temp_max": np.nan,
            "temp_mean": np.nan,
            "days_btwn_events_mean": np.nan,
            "days_btwn_events_min": np.nan,
            "duration_max": np.nan,
            "duration_mean": np.nan,
            "duration_sum": np.nan,
        },
        index=[0],
    )

    ## Attach grouping variable for each wave segment
    # Create a boolean series where True indicates non-NA values
    non_na = group_in["heatw"].notna()

    # Use cumsum on the negated non_na series to form groups
    group = (~non_na).cumsum()

    # Retain group numbers only for non-NA rows
    group_in["group"] = group.where(non_na, np.nan)

    # Drop rows where '"heatw"' is NaN
    filtered_df = group_in.dropna(subset=["heatw"]).copy()

    # Group by 'wave_id'
    grouped = filtered_df.groupby("group")
    # for g in grouped:
    # display(grouped.get_group(g[0]))

    # Get number of events
    n_events = len(grouped)

    if len(filtered_df) == 0:
        df_out["n_events"] = 0

    else:
        # Calculate the size of each group
        wave_sizes = grouped.size()

        # Calculate required statistics
        duration_max = wave_sizes.max()
        duration_mean = wave_sizes.mean()
        duration_sum = wave_sizes.sum()

        # Calculate the difference in days between consecutive waves
        start_dates = pd.DataFrame(grouped["date"].min()).rename(
            columns={"date": "start_date"}
        )
        end_dates = pd.DataFrame(grouped["date"].max()).rename(
            columns={"date": "end_date"}
        )
        # Shift rows of end dates one down to calculate time from last end to next start easier
        end_dates = end_dates.shift(1)

        df_dates = pd.concat([start_dates, end_dates], axis=1)[
            ["start_date", "end_date"]
        ]

        # Get time differences from end_date to start_date
        df_dates["time_diff"] = df_dates["start_date"] - df_dates["end_date"]

        # Turn duration into integer
        df_dates["time_diff"] = df_dates["time_diff"].dt.days

        # Calculate the average and minimum time interval
        days_btwn_events_mean = df_dates["time_diff"].mean()
        days_btwn_events_min = df_dates["time_diff"].min()

        # Extract the mean and max extreme temperature
        if wave_type == "heatwave":
            temp_var = "maxtmp"
        else:
            temp_var = "mintmp"

        temp_min = filtered_df[temp_var].min()
        temp_max = filtered_df[temp_var].max()
        temp_mean = filtered_df[temp_var].mean()

        # Overwrite data
        df_out["n_events"] = n_events

        df_out["temp_min"] = temp_min
        df_out["temp_max"] = temp_max
        df_out["temp_mean"] = temp_mean

        df_out["duration_max"] = duration_max
        df_out["duration_mean"] = duration_mean
        df_out["duration_sum"] = duration_sum

        df_out["days_btwn_events_mean"] = days_btwn_events_mean
        df_out["days_btwn_events_min"] = days_btwn_events_min

    # Add prefix based on wave type
    if wave_type == "heatwave":
        df_out = df_out.add_prefix("hw_")
    else:
        df_out = df_out.add_prefix("cw_")

    return df_out


# Function to divide the list of dataframes into 10 nearly equal parts
def divide_chunks(lst, n):
    for i in range(0, len(lst), n):
        yield lst[i : i + n]


# Function to put the dataframes into 10 lists by keeping the idp together
def put_df_into_10lists_by_keeping_idp_together(df_in):
    dfs = [g for _, g in df_in.groupby("idp")]

    # Size of each chunk (100 for 1000 dataframes)
    chunk_size = len(dfs) // 10

    # Splitting the list of dataframes into 10 parts
    dfs_chunks = list(divide_chunks(dfs, chunk_size))

    # Concatenate dataframes within each chunk
    concatenated_dfs = [pd.concat(chunk, ignore_index=True) for chunk in dfs_chunks]

    return concatenated_dfs


# ==========================================================================================
def ___SAFRAN___():
    pass


# ==========================================================================================
def safran_extract_value(ds_in, lat_in, lon_in, return_fig=False):
    """
    Extracts a value from a safran netcdf dataset (subsetted to variable AND time) at a given lat/lon point.
    🔗 Function is based on this StackOverflow: https://stackoverflow.com/questions/58758480/xarray-select-nearest-lat-lon-with-multi-dimension-coordinates?rq=1
    """

    # First, find the index of the grid point nearest the inputted lat/lon.
    abslat = np.abs(ds_in.lat - lat_in)
    abslon = np.abs(ds_in.lon - lon_in)
    c = np.maximum(abslon, abslat)
    ([latloc], [lonloc]) = np.where(c == np.min(c))

    # Now I can use that index location to get the values at the x/y diminsion
    point_ds = ds_in.isel(lon=lonloc, lat=latloc)

    if return_fig:
        # Make plot with two figures side by side
        fig, axs = plt.subplots(1, 2, figsize=(15, 5))

        # Plot for full France
        # axs[0].scatter(ds_in.lon, ds_in.lat)
        ds_in.plot(x="lon", y="lat", ax=axs[0])
        # Plot requested lat/lon point blue
        axs[0].scatter(lon_in, lat_in, color="b")
        axs[0].text(lon_in, lat_in, "requested")
        # Plot nearest point in the array red
        axs[0].scatter(point_ds.lon, point_ds.lat, color="r")
        axs[0].text(point_ds.lon, point_ds.lat, "nearest")
        axs[0].set_title("Value at nearest point: %s" % point_ds.data)

        # Plot zoomed into extracted point
        # Plot grid around extracted poin
        ds_sub = ds_in[latloc - 4 : latloc + 4, lonloc - 4 : lonloc + 4]
        ds_sub.plot(x="lon", y="lat", ax=axs[1])
        axs[1].scatter(ds_sub.lon, ds_sub.lat, color="white")
        # Plot requested lat/lon point blue
        axs[1].scatter(lon_in, lat_in, color="b")
        axs[1].text(lon_in, lat_in, "requested")
        # Plot nearest point in the array red
        axs[1].scatter(point_ds.lon, point_ds.lat, color="r")
        axs[1].text(point_ds.lon, point_ds.lat, "nearest")
        axs[1].set_title("Value at nearest point: %s" % point_ds.data)

    return point_ds.data.item()


def safran_get_closest_point(ds_in, lat_in, lon_in):
    """
    Extracts a value from a safran netcdf dataset (subsetted to variable AND time) at a given lat/lon point.
    """

    # First, find the index of the grid point nearest the inputted lat/lon.
    abslat = np.abs(ds_in.lat - lat_in)
    abslon = np.abs(ds_in.lon - lon_in)
    c = np.maximum(abslon, abslat)
    ([latloc], [lonloc]) = np.where(c == np.min(c))

    return latloc, lonloc


def safran_extract_from_index(ds_in, latloc, lonloc):
    """
    Extracts a value from a safran netcdf dataset (subsetted to variable AND time) at a given lat/lon point.
    """

    # Now I can use that index location to get the values at the x/y diminsion
    point_ds = ds_in.isel(lon=lonloc, lat=latloc)

    return point_ds.data.item()


def safran_extract_data_per_site(
    nc_filepath, sites_in, timestep, only_means=False, verbose=False
):
    # Open netcdf dataset
    ds_org = xr.open_dataset(nc_filepath)
    # List of all variables
    # - Tair
    # - Qair
    # - PSurf
    # - Wind
    # - Rainf
    # - Snowf
    # - SWdown
    # - LWdown

    # Attach new variables
    # - Total Precipitation
    ds_org["Precip"] = ds_org["Rainf"] + ds_org["Snowf"]
    # - Saturation Vapor Pressure
    ds_test["SVP"] = 0.6108 * np.exp(
        (17.27 * ds_test["Tair"]) / (ds_test["Tair"] + 237.3)
    )
    # - Actual Vapor Pressure
    ds_test["AVP"] = (ds_test["Qair"] * ds_test["PSurf"]) / (
        0.622 + 0.378 * ds_test["Qair"]
    )
    # - Vapor Pressure Deficit
    ds_test["VPD"] = ds_test["SVP"] - ds_test["AVP"]

    # Get ds of means
    ds_means = (
        ds_org[
            [
                "Tair",
                "VPD",
                "SWdown",
                "LWdown",
                "PSurf",
                "Wind",
            ]
        ]
        .groupby(f"tstep.{timestep}")
        .mean("tstep")
    )

    # Get ds of min
    ds_min = (
        ds_org[
            [
                "Tair",
                "PSurf",
            ]
        ]
        .groupby(f"tstep.{timestep}").
        .min("tstep")
    )
    
    # Get ds of max
    ds_max = (
        ds_org[
            [
                "Tair",
                "VPD",
                "PSurf",
                "Wind",
            ]
        ]
        .groupby(f"tstep.{timestep}").
        .max("tstep")
    )
    
    # Get ds of sum
    ds_sum = (
        ds_org[
            [
                "Precip",
            ]
        ]
        .groupby(f"tstep.{timestep}")
        .sum("tstep")
    )

    # Loop over coordinates
    for i in tqdm(range(sites_in.shape[0]), disable=not verbose):
        # Get closest point
        lat = sites_in["y"].iloc[i]
        lon = sites_in["x"].iloc[i]
        latloc, lonloc = safran_get_closest_point(ds_org, lat, lon)
        # EXTRACT DATA
        # DAILY MEANS ---------------------------------------------------------------
        ds_daily_mean = ds_org.groupby("tstep.dayofyear").mean("tstep")

        # Temperature
        if verbose:
            print(" - Extracting daily mean temperature...")
        df_tair_mean = safran_extract_all_timesteps(
            ds_daily_mean["Tair"],
            "tair_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        # Humidity
        if verbose:
            print(" - Extracting daily mean humidity...")
        df_hum_mean = safran_extract_all_timesteps(
            ds_daily_mean["Qair"],
            "qair_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        # Wind
        if verbose:
            print(" - Extracting daily mean wind...")
        df_wind_mean = safran_extract_all_timesteps(
            ds_daily_mean["Wind"],
            "wind_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        # Radiation
        if verbose:
            print(" - Extracting daily mean short-wave radiation...")
        df_rad_mean = safran_extract_all_timesteps(
            ds_daily_mean["SWdown"],
            "rad_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        if verbose:
            print(" - Extracting daily mean long-wave radiation...")
        df_rad_mean = safran_extract_all_timesteps(
            ds_daily_mean["LWdown"],
            "rad_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        # Precipitation
        if verbose:
            print(" - Extracting daily mean rainfall...")
        df_prec_mean = safran_extract_all_timesteps(
            ds_daily_mean["Rainf"],
            "prec_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        # Pressure
        if verbose:
            print(" - Extracting daily mean pressure...")
        df_press_mean = safran_extract_all_timesteps(
            ds_daily_mean["PSurf"],
            "press_mean",
            "dayofyear",
            latloc,
            lonloc,
            verbose=verbose,
        )

        if only_means:
            df_out = pd.concat(
                [
                    df_tair_mean,
                    df_hum_mean.drop(columns="dayofyear"),
                    df_wind_mean.drop(columns="dayofyear"),
                    df_rad_mean.drop(columns="dayofyear"),
                    df_prec_mean.drop(columns="dayofyear"),
                    df_press_mean.drop(columns="dayofyear"),
                ],
                axis=1,
            )

        else:
            # DAILY MINIMUMS ---------------------------------------------------------------
            ds_daily_min = ds_org.groupby("tstep.dayofyear").min("tstep")
            # Temperature
            if verbose:
                print(" - Extracting daily minimum temperature...")
            df_tair_min = safran_extract_all_timesteps(
                ds_daily_min["Tair"],
                "tair_min",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Humidity
            if verbose:
                print(" - Extracting daily minimum humidity...")
            df_hum_min = safran_extract_all_timesteps(
                ds_daily_min["Qair"],
                "qair_min",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Pressure
            if verbose:
                print(" - Extracting daily minimum pressure...")
            df_press_min = safran_extract_all_timesteps(
                ds_daily_min["PSurf"],
                "press_min",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # DAILY MAXIMUMS ---------------------------------------------------------------
            ds_daily_max = ds_org.groupby("tstep.dayofyear").max("tstep")
            # Temperature
            if verbose:
                print(" - Extracting daily maximum temperature...")
            df_tair_max = safran_extract_all_timesteps(
                ds_daily_max["Tair"],
                "tair_max",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Humidity
            if verbose:
                print(" - Extracting daily maximum humidity...")
            df_hum_max = safran_extract_all_timesteps(
                ds_daily_max["Qair"],
                "qair_max",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Pressure
            if verbose:
                print(" - Extracting daily maximum pressure...")
            df_press_max = safran_extract_all_timesteps(
                ds_daily_max["PSurf"],
                "press_max",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Wind
            if verbose:
                print(" - Extracting daily maximum wind...")
            df_wind_max = safran_extract_all_timesteps(
                ds_daily_max["Wind"],
                "wind_max",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # DAILY SUMS ---------------------------------------------------------------
            ds_daily_sum = ds_org.groupby("tstep.dayofyear").sum("tstep")
            # Rainfall
            if verbose:
                print(" - Extracting daily sum rainfall...")
            df_prec_sum = safran_extract_all_timesteps(
                ds_daily_sum["Rainf"],
                "rainf_sum",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Snowfall
            if verbose:
                print(" - Extracting daily sum snowfall...")
            df_snow_sum = safran_extract_all_timesteps(
                ds_daily_sum["Snowf"],
                "snowf_sum",
                "dayofyear",
                latloc,
                lonloc,
                verbose=verbose,
            )

            # Merge dataframes
            df_out = pd.concat(
                [
                    df_tair_mean,
                    df_tair_min.drop(columns="dayofyear"),
                    df_tair_max.drop(columns="dayofyear"),
                    df_hum_mean.drop(columns="dayofyear"),
                    df_hum_min.drop(columns="dayofyear"),
                    df_hum_max.drop(columns="dayofyear"),
                    df_wind_mean.drop(columns="dayofyear"),
                    df_wind_max.drop(columns="dayofyear"),
                    df_rad_mean.drop(columns="dayofyear"),
                    df_prec_mean.drop(columns="dayofyear"),
                    df_press_mean.drop(columns="dayofyear"),
                    df_press_min.drop(columns="dayofyear"),
                    df_press_max.drop(columns="dayofyear"),
                    df_prec_sum.drop(columns="dayofyear"),
                    df_snow_sum.drop(columns="dayofyear"),
                ],
                axis=1,
            )
        # End else only_means
        df_out["idp"] = sites_in["idp"].iloc[i]

        if i == 0:
            df_final = df_out.copy()
        else:
            df_final = pd.concat([df_final, df_out], axis=0)
    # End of loop over sites

    return df_final


def safran_extract_all_timesteps(
    ds_var, my_var, time_var, latloc, lonloc, verbose=False
):
    """
    Extract the data for a given variable at all timesteps.
    Input data must be reduced to one variable.
    time_var is the variable name for the time dimension.
    my_var is the variable name (only used to label df_out column).
    latloc and lonloc are the indeces for the closest pixel to the site.
    """
    # Set up output dataframe
    df_out = pd.DataFrame(columns=[time_var, my_var])
    # Get time steps
    all_tsteps = ds_var.get_index(time_var).to_list()
    # Loop through all timesteps)
    for i in tqdm(
        range(len(all_tsteps)),
        disable=not verbose,
        desc=f"        - Working on timestep ({time_var}): ",
    ):
        # Get ds for current timestep
        ds_var_tstep = ds_var[i, :, :]
        df_i = pd.DataFrame(
            {
                time_var: [all_tsteps[i]],
                my_var: [safran_extract_from_index(ds_var_tstep, latloc, lonloc)],
            }
        )

        df_out = pd.concat([df_out, df_i], axis=0)

    return df_out
