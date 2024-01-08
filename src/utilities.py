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
import xarray as xr


def ___GENERAL_FUNCTIONS___():
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
    files = glob.glob(f"{here('data/tmp/nfi/from-R')}/*_nfi_dataset_raw.csv")

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
def get_final_nfi_coordinates(noisy_or_corrected=None, geojson_or_csv=None, epsg=None):
    
    if noisy_or_corrected == "noisy":
        if geojson_or_csv == "csv":
            print("Loading noisy coordinates from csv.")
            return pd.read_csv(here("data/final/nfi/coords_of_sites_with_idp.csv"), index_col=None)
        elif geojson_or_csv == "geojson":
            if epsg == "4326":
                print("Loading noisy coordinates from geojson EPSG 4326.")
                return gpd.read_file(here("data/final/nfi/sites_with_idp_epsg4326.geojson"))
            elif epsg == "2154":
                print("Loading noisy coordinates from geojson EPSG 2154.")
                return gpd.read_file(here("data/final/nfi/sites_with_idp_epsg2154.geojson"))
            else:
                raise ValueError("Please specify EPSG 4326 or 2154.")
        else:
            raise ValueError("Please specify if you want to load geojson or csv.")
        
    if noisy_or_corrected == "corrected":
        raise ValueError("Getting corrected coordinates implemented yet.")
    
def get_final_nfi_data_for_analysis():
    return pd.read_feather(here("data/final/nfi/nfi_ready_for_analysis.feather"))

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

    raise ValueError(
        "This function is depreciated. Use calculate_growth_mortality_optimized() instead."
    )

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
# Turn of formatting just for this function
# fmt: off
def calculate_growth_mortality_optimized(
    df_in,
    verbose=False,
    grouping_variable="idp",
    per_year=True,
    min_trees_per_plot=None,
):
    """
    Optimized function to calculate growth and mortality metrics for a given input df.

    Args:
        df_in (pd.DataFrame): Input dataframe.
        verbose (bool, optional): Verbose output. Defaults to False.
        divide_by_nplots (bool, optional): Divide metrics by number of plots. Defaults to False.
        grouping_variable (str, optional): Grouping variable name (grouping done outside, not within the function!). Defaults to "idp".
        per_year (bool, optional): Calculate metrics per year. Defaults to True.
        min_trees_per_plot (int, optional): Minimum trees per plot. Defaults to 1.

    Raises:
        ValueError: If the grouping variable is not in the dataframe.

    Returns:
        pd.DataFrame: Mortality metrics per group.
    """

    # Old input check
        # Check if metrics should be calculated based on min_trees_per_plot
    if min_trees_per_plot is not None and n_ini < min_trees_per_plot:
        raise ValueError(f"Depreciated function. Do filtering based on n_trees_per_plot AFTER calling this function.")

    # Check if grouping variable is valid
    if grouping_variable not in df_in.columns:
        raise ValueError(f"Grouping variable {grouping_variable} is not in the dataframe.")

    # Get grouping variable
    my_group = df_in[grouping_variable].unique()[0]

    if verbose:
        print(f"> Calculating growth and mortality for {grouping_variable}: {my_group}.")

    # Set timespan
    timespan = 1 / 5 if per_year else 1

    # Remove all trees that were dead at the first visit
    df_in = df_in[df_in['tree_state_1'] != 'dead']
    
    # Subset to relevant columns
    relevant_cols = list(set([grouping_variable, "idp", "tree_id", "tree_state_1", "tree_state_2", "tree_state_change", "ba_1", "ba_2", "v"]))
    df_in = df_in[relevant_cols]

    # Compute conditions once
    alive_1     = df_in['tree_state_1']      == 'alive'
    alive_2     = df_in['tree_state_2']      == 'alive'
    alive_alive = df_in['tree_state_change'] == 'alive_alive'
    alive_dead  = df_in['tree_state_change'] == 'alive_dead'
    alive_cut   = df_in['tree_state_change'] == 'alive_cut'
    new_alive   = df_in['tree_state_change'] == 'new_alive'

    # Stem-based metrics
    # Define stand-metrics dictionary
    stem_metrics = pd.Series({
        "n_a1": df_in[alive_1].shape[0],     # a1 = alive at first visit
        "n_a2": df_in[alive_2].shape[0],     # a2 = alive at second visit                              
        "n_aa": df_in[alive_alive].shape[0], # aa = alive_alive       
        "n_ad": df_in[alive_dead].shape[0],  # ad = alive_dead       
        "n_ac": df_in[alive_cut].shape[0],   # ac = alive_cut   
        "n_na": df_in[new_alive].shape[0],   # na = new_alive   
    })
        
    # ! Notes:
    # ! 1.) I am calculation mortality only with respect to the first visit so that we know how much of the initial stand was lost per year. This means that I can also calculate metrics wrt. volume!
    # ! 2.) The mortality calculations from Hoshino and Esquivel are very similar, so I am only using the Esquivel one.
    # !     - Esquivel: 1 - (survivors / initals) ** (1 / 5) * 100
    # !     - Hoshino: ln(initals / survivors) / 5 * 100
    
    # Size-based metrics
    size_metrics = df_in[["ba_1", "ba_2", "v"]].assign(
        # Basal Area
        ba_ax_v1 = alive_1 * df_in["ba_1"],
        ba_ax_v2 = alive_2 * df_in["ba_2"],
        
        ba_aa_v1 = alive_alive * df_in["ba_1"],
        ba_aa_v2 = alive_alive * df_in["ba_2"],
        
        ba_ad_v1 = alive_dead * df_in["ba_1"],
        ba_ac_v1 = alive_cut * df_in["ba_1"],
        ba_na_v2 = new_alive * df_in["ba_2"],
        
        # Volume
        vol_ax_v1 = alive_1 * df_in["v"],
        vol_aa_v1 = alive_alive * df_in["v"],
        vol_ad_v1 = alive_dead * df_in["v"],
        vol_ac_v1 = alive_cut * df_in["v"],
        
    ).sum().drop(["ba_1", "ba_2", "v"], axis=0)

    # Merge stand metrics and size-based metrics
    sm = pd.concat([stem_metrics, size_metrics])

    # Calculate metrics
    metrics_of_change = pd.Series({
        # Mortality
        # - Stem-Based
        #   - Esquivel Equation: 1 - (survivors / initals) ** (1 / 5) | How many trees die per year with respect to current population?
        #   - To separate natural mortality, we need to add the number of trees that were cut to the survivors because they "survived nature"
        #     And vice versa for the number of trees that were cut.
        "mort_tot_stems_prc_yr_esq" : np.nan if sm.n_a1 == 0 else (1 - (sm.n_aa / sm.n_a1) ** timespan) * 100,  # Total (natural and human)
        "mort_nat_stems_prc_yr_esq" : np.nan if sm.n_a1 == 0 else (1 - ((sm.n_aa + sm.n_ac) / sm.n_a1) ** timespan) * 100,  # Natural
        "mort_cut_stems_prc_yr_esq" : np.nan if sm.n_a1 == 0 else (1 - ((sm.n_aa + sm.n_ad) / sm.n_a1) ** timespan) * 100,  # Human Cutting
        
        #   - Simple: How many trees died per year with respect to initial population?
        #     Here the logic is reversed to the Esquivel equation, because taking the percentage of nd to initial trees
        "mort_tot_stems_prc_yr" :     np.nan if sm.n_a1 == 0 else (sm.n_ad + sm.n_ac) / sm.n_a1 * timespan * 100,  # Total (natural and human)
        "mort_nat_stems_prc_yr" :     np.nan if sm.n_a1 == 0 else sm.n_ad / sm.n_a1 * timespan * 100,  # Natural
        "mort_cut_stems_prc_yr" :     np.nan if sm.n_a1 == 0 else sm.n_ac / sm.n_a1 * timespan * 100,  # Human Cutting
        
        # - Size-Based
        #   Question: Of mortality process types (total, natural, cutting), how much of the initial size alive of subset was lost to that process?
        #   Note that ba_v1_a is the same as summing up ba_aa_v1, ba_ad_v1, ba_ac_v1
        #   - Basal Area
        "mort_tot_ba_yr" :                                  (sm.ba_ad_v1 + sm.ba_ac_v1) * timespan,
        "mort_tot_ba_prc_yr" : np.nan if sm.ba_ax_v1 == 0 else (sm.ba_ad_v1 + sm.ba_ac_v1) * timespan / sm.ba_ax_v1 * 100,
        
        "mort_nat_ba_yr" :                                  sm.ba_ad_v1 * timespan,
        "mort_nat_ba_prc_yr" : np.nan if sm.ba_ax_v1 == 0 else sm.ba_ad_v1 * timespan / sm.ba_ax_v1 * 100,
        
        "mort_cut_ba_yr" :                                  sm.ba_ac_v1 * timespan,
        "mort_cut_ba_prc_yr" : np.nan if sm.ba_ax_v1 == 0 else sm.ba_ac_v1 * timespan / sm.ba_ax_v1 * 100,
        
        #  - Volume
        "mort_tot_vol_yr" :                                   (sm.vol_ad_v1 + sm.vol_ac_v1) * timespan,
        "mort_tot_vol_prc_yr" : np.nan if sm.vol_ax_v1 == 0 else (sm.vol_ad_v1 + sm.vol_ac_v1) * timespan / sm.vol_ax_v1 * 100,
        
        "mort_nat_vol_yr" :                                   sm.vol_ad_v1 * timespan,
        "mort_nat_vol_prc_yr" : np.nan if sm.vol_ax_v1 == 0 else sm.vol_ad_v1 * timespan / sm.vol_ax_v1 * 100,
        
        "mort_cut_vol_yr" :                                   sm.vol_ac_v1 * timespan,
        "mort_cut_vol_prc_yr" : np.nan if sm.vol_ax_v1 == 0 else sm.vol_ac_v1 * timespan / sm.vol_ax_v1 * 100,
        
        # Growth (de-coupled from mortality!)
        # - Stem-Based | Hoshino Equation: ln(finals / survivors) / 5
        "grwt_stems_prc_yr" : np.nan if sm.n_aa == 0 else np.log(sm.n_a2 / sm.n_aa) * timespan * 100,

        # - Size-Based (only doable for basal area, because volume is not available for the second visit)
        #   - Total Growth
        "grwt_tot_ba_yr":                                         (sm.ba_aa_v2 - sm.ba_aa_v1 + sm.ba_na_v2) * timespan, # Difference of survivors between v1 and v2 plus all of v2 of new trees
        "grwt_tot_ba_prc_yr":     np.nan if sm.ba_aa_v1 == 0 else (sm.ba_aa_v2 - sm.ba_aa_v1 + sm.ba_na_v2) * timespan / sm.ba_aa_v1 * 100,
        "grwt_tot_ba_prc_yr_hos": np.nan if sm.ba_aa_v1 == 0 else np.log((sm.ba_aa_v2 + sm.ba_na_v2) / sm.ba_aa_v1) * timespan * 100,
        
        "grwt_sur_ba_yr":                                         (sm.ba_aa_v2 - sm.ba_aa_v1) * timespan,
        "grwt_sur_ba_prc_yr":     np.nan if sm.ba_aa_v1 == 0 else (sm.ba_aa_v2 - sm.ba_aa_v1) * timespan / sm.ba_aa_v1 * 100,
        "grwt_sur_ba_prc_yr_hos": np.nan if sm.ba_aa_v1 == 0 else np.log(sm.ba_aa_v2 / sm.ba_aa_v1) * timespan * 100,
        
        "grwt_rec_ba_yr":                                         sm.ba_na_v2 * timespan,
        "grwt_rec_ba_prc_yr":     np.nan if sm.ba_aa_v1 == 0 else sm.ba_na_v2 * timespan / sm.ba_aa_v1 * 100,
        
        # Change of alive biomass (How has the total alive biomass changed over time?)
        "change_tot_ba_yr":                                       (sm.ba_ax_v2 - sm.ba_ax_v1) * timespan,
        "change_tot_ba_prc_yr":   np.nan if sm.ba_ax_v1 == 0 else (sm.ba_ax_v2 - sm.ba_ax_v1) * timespan / sm.ba_ax_v1 * 100,
    })

    # Combine all metrics
    df_out = pd.DataFrame({grouping_variable: my_group, 
                           "n_plots": df_in["idp"].nunique()},index=[0])
    df_out = df_out.assign(**sm, **metrics_of_change)
    df_out.insert(0, "n_plots", df_out.pop("n_plots"))
    df_out.insert(0, grouping_variable, df_out.pop(grouping_variable))

    return df_out


# Example usage (Note: df_in should be a properly formatted pandas DataFrame)
# df_out = calculate_growth_mortality_optimized(df_in)
# Turn formatting on again
# fmt: on
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


def ___MAKE_MAPS___():
    pass

def make_plots_per_file_parallel(
    file_group,
    run_only_subset=True,
    subset_fraction=None,
    method=None,
    verbose=False,
):
    for my_file in file_group:
        make_plots_per_file(
            my_file=my_file,
            method=method,
            run_only_subset=run_only_subset,
            subset_fraction=subset_fraction,
            verbose=verbose,
        )

def make_plots_per_file(my_file, method, run_only_subset, subset_fraction, verbose=False):
    # ! Preparation ---------------------------------------------------------------
    if method != "direct":
        raise ValueError("Only direct method is currently implemented")

    sp_france = get_shp_of_region("cty")  # Get shapefile of france

    # Extract species and region from filename
    my_species = my_file.split("/")[-1].split("_")[1].split("-")[0]
    my_region = my_file.split("/")[-1].split("_")[-1].split(".")[0].split("-")[1]

    # Set dir for plots
    # ? Debugging: Change output folder...
    if run_only_subset:
        my_dir = here(
            f"python/00_process_nfi_data/maps_of_change/{method}/subset_{round(subset_fraction*100)}%_of_sites/species-{my_species}_region-{my_region}"
        )
    else:
        my_dir = here(
            f"python/00_process_nfi_data/maps_of_change/{method}/species-{my_species}_region-{my_region}"
        )

    # Create dir if not exists
    os.makedirs(my_dir, exist_ok=True)

    # ! Read file  ---------------------------------------------------------------
    df_loop = pd.read_feather(my_file)

    # todo: If method is not direct, then I have to rename variables so that _sd are dropped and _mean removed from variable name!

    # Plot distribution of n_plots per region_year
    fig, ax = plt.subplots(figsize=(12, 4))
    df_loop["n_plots"].plot(kind="hist", bins=50, ax=ax).set_title(
        f"Distribution of n_plots per {my_region} and year"
    )
    plt.savefig(
        f"{my_dir}/_distribution_of_nplots.png",
        dpi=300,
    )
    plt.close(fig)

    # ! Prepare plotting data ---------------------------------------------------------------
    # Get shapefile and clean merging variables
    sp_loop = get_shp_of_region(my_region)
    sp_loop["year"] = sp_loop["year"].astype(int)
    sp_loop[my_region] = sp_loop[my_region].astype(str)
    sp_loop[f"{my_region}_year"] = (
        sp_loop[f"{my_region}"] + "_" + sp_loop["year"].astype(str)
    )
    sp_loop = sp_loop.reset_index(drop=True)

    # Clean merging variables in df_loop
    df_loop[my_region] = df_loop[f"{my_region}_year"].str.split("_").str[0]
    df_loop["year"] = df_loop[f"{my_region}_year"].str.split("_").str[1].astype(int)
    df_loop[f"{my_region}_year"] = (
        df_loop[f"{my_region}"] + "_" + df_loop["year"].astype(str)
    )
    df_loop = df_loop.reset_index(drop=True)

    # Attach df to shapefile
    df_loop = df_loop.merge(
        sp_loop, how="right", on=[f"{my_region}_year", "year", my_region]
    )
    df_loop = df_loop.reset_index(drop=True)
    df_loop = gpd.GeoDataFrame(df_loop, geometry="geometry")

    # ! Calculate change relative to 2010 ---------------------------------------------------
    # Get metrics
    all_metrics = (
        # [x for x in df_loop.columns if "mort" in x]
        [x for x in df_loop.columns if "grwt" in x]
        + [x for x in df_loop.columns if "change" in x]
    )

    # Loop over every region-tile
    diff_abs = pd.DataFrame()
    diff_rel = pd.DataFrame()

    for i_region in df_loop[my_region].unique():
        # Get the data for the current region-tile
        idf_region = df_loop[df_loop[my_region] == i_region]
        # display(idf_region.n_plots.isna().sum())
        # display(idf_region.shape[0])

        # Get the data for the year 2010, subset to metrics
        data_2010 = idf_region[idf_region["year"] == 2010].reset_index(drop=True)[
            all_metrics
        ]

        # If there is no data for 2010, skip the region-tile
        # if data_2010.dropna().shape[0] == 0:
        #     continue

        # Loop over all years
        for i_year in df_loop["year"].unique():
            # Skip the year 2010
            if i_year == 2010:
                continue
            # Get the data for the current year, subset to metrics
            data_year = idf_region[idf_region["year"] == i_year].reset_index(drop=True)[
                all_metrics
            ]

            # If there is no data for the current year, skip it
            if data_year.dropna().shape[0] == 0:
                continue

            # if i_year == 2012:
            #     raise ValueError("Debugging Stop")

            # Calculate the absolute and relative differences
            i_abs = data_year.subtract(data_2010)
            i_rel = i_abs.divide(data_2010) * 100

            # Add the region and year to the results
            i_abs[my_region] = i_region
            i_abs["year"] = i_year
            i_abs[f"{my_region}_year"] = f"{i_region}_{i_year}"

            i_rel[my_region] = i_region
            i_rel["year"] = i_year
            i_rel[f"{my_region}_year"] = f"{i_region}_{i_year}"

            # Add the results to the main DataFrames
            diff_abs = pd.concat([diff_abs, i_abs], ignore_index=True)
            diff_rel = pd.concat([diff_rel, i_rel], ignore_index=True)

    # Clean infities and index
    diff_rel = diff_rel.replace([np.inf, -np.inf], np.nan)
    diff_abs.reset_index(drop=True)
    diff_rel.reset_index(drop=True)

    # Subset shapefile
    sp_tmp = sp_loop.query("year != 2010")[["year", f"{my_region}_year", "geometry"]]

    # Attach geometry data
    diff_abs_sp = diff_abs.merge(sp_tmp, on=["year", f"{my_region}_year"], how="right")
    diff_rel_sp = diff_rel.merge(sp_tmp, on=["year", f"{my_region}_year"], how="right")

    # ! Loop over all metrics ---------------------------------------------------------------
    idebug = 0
    for my_var in tqdm(all_metrics, disable=True):
        # print(f" - Working on: {my_var} | {my_dir}", end=" | ")

        # Get figure dictionary for variable
        fig_dic = figure_dictionary_for_variable(my_var, my_species)

        # * Plot normal yearly figure
        make_map_of_change(
            df_loop,
            fig_dic,
            sp_france,
            my_dir,
        )

        # * Absolute change
        make_map_of_change(
            diff_abs_sp,
            figure_dictionary_for_variable(
                my_var, my_species, change_to_2010="absolute"
            ),
            sp_france,
            my_dir,
        )

        # * Relative change
        make_map_of_change(
            diff_rel_sp,
            figure_dictionary_for_variable(
                my_var, my_species, change_to_2010="relative"
            ),
            sp_france,
            my_dir,
        )

        idebug = idebug + 1
        # if idebug > 3:
        #     break
            # raise ValueError("Debugging Stop")

def figure_dictionary_for_variable(my_var, my_species, change_to_2010=None):
    # Variable
    fig_dic = {"var": my_var}

    # * Title
    # Species
    if my_species == "all":
        fig_dic["species"] = "All Species"
    else:
        fig_dic["species"] = my_species.capitalize()

    # Mortality
    if "mort" in my_var:
        if change_to_2010 is None:
            fig_dic["cmap"] = plt.cm.Reds
        else:
            fig_dic["cmap"] = plt.cm.RdBu_r
        if "tot" in my_var:
            fig_dic["change"] = "Total Loss"
            fig_dic["main"] = fig_dic["change"] + " of " + fig_dic["species"]
        elif "nat" in my_var:
            fig_dic["change"] = "Mortality"
            fig_dic["main"] = fig_dic["change"] + " of " + fig_dic["species"]
        elif "cut" in my_var:
            fig_dic["change"] = "Harvest"
            fig_dic["main"] = fig_dic["change"] + " of " + fig_dic["species"]

    # Growth
    if "grwt" in my_var:
        fig_dic["change"] = "Gain"
        if change_to_2010 is None:
            fig_dic["cmap"] = plt.cm.Greens
        else:
            fig_dic["cmap"] = plt.cm.RdBu
        if "tot" in my_var:
            fig_dic["main"] = f"Total Growth of " + fig_dic["species"]
        elif "sur" in my_var:
            fig_dic["main"] = f"Survivor Growth of " + fig_dic["species"]
        elif "rec" in my_var or "stems" in my_var:
            fig_dic["main"] = f"Recruits Growth of " + fig_dic["species"]

    # Change
    if "change" in my_var:
        fig_dic["change"] = "Change"
        fig_dic["cmap"] = plt.cm.RdBu
        fig_dic["main"] = f"Total Change of Alive Biomass of " + fig_dic["species"]
        if change_to_2010 in ["absolute", "relative"]:
            fig_dic[
                "main"
            ] = "Ignore this plot (showing change of change in alive biomass)"

    # If change to 2010, then change title
    if change_to_2010 == "absolute":
        fig_dic["main"] = "Absolute Change of " + fig_dic["main"]

    if change_to_2010 == "relative":
        fig_dic["main"] = "Relative Change of " + fig_dic["main"]

    # * Colorbar
    if "change" in my_var or change_to_2010 is not None:
        fig_dic["default_cbar"] = False
    else:
        fig_dic["default_cbar"] = True

    # * Legend
    fig_dic["legend"] = fig_dic["change"]
    if "stems_prc_yr" in my_var:
        fig_dic["legend"] = fig_dic["legend"] + " [%-stems yr$^{-1}$]"
    elif "ba_yr" in my_var:
        fig_dic["legend"] = fig_dic["legend"] + " [m$^{2}$ yr$^{-1}$]"
    elif "ba_yr_prc":
        fig_dic["legend"] = fig_dic["legend"] + " [%-m$^{2}$ yr$^{-1}$]"
    elif "vol_yr" in my_var:
        fig_dic["legend"] = fig_dic["legend"] + " [m$^{3}$ yr$^{-1}$]"
    elif "vol_yr_prc":
        fig_dic["legend"] = fig_dic["legend"] + " [%-m$^{3}$ yr$^{-1}$]"

    if change_to_2010 is not None:
        # Pick if relative or absolute
        pref = "Absolute" if change_to_2010 == "absolute" else "Relative"
        prct = "% of " if change_to_2010 == "relative" else ""
        fig_dic["legend"] = pref + " Change of " + fig_dic["change"]
        if "stems_prc_yr" in my_var:
            fig_dic["legend"] = fig_dic["legend"] + f" [{prct}%-stems yr$^{-1}$]"
        elif "ba_yr" in my_var:
            fig_dic["legend"] = fig_dic["legend"] + f" [{prct}m$^{2}$ yr$^{-1}$]"
        elif "ba_yr_prc":
            fig_dic["legend"] = fig_dic["legend"] + f" [{prct}%-m$^{2}$ yr$^{-1}$]"
        elif "vol_yr" in my_var:
            fig_dic["legend"] = fig_dic["legend"] + f" [{prct}m$^{3}$ yr$^{-1}$]"
        elif "vol_yr_prc":
            fig_dic["legend"] = fig_dic["legend"] + f" [{prct}%-m$^{3}$ yr$^{-1}$]"

    # * Plot directory
    fig_dic["dir"] = "no_change"
    if change_to_2010 == "absolute":
        fig_dic["dir"] = "absolute_change"
    if change_to_2010 == "relative":
        fig_dic["dir"] = "relative_change"

    return fig_dic


def make_map_of_change(
    df_in,
    fig_dic,
    sp_france,
    fig_dir,
    overwrite=True,
):
    import matplotlib.pyplot as plt
    import matplotlib.gridspec as gridspec
    import geopandas as gpd
    import numpy as np
    import matplotlib.colors as colors
    import matplotlib.colors as mcolors
    from matplotlib.colors import LinearSegmentedColormap

    # ! DEBUGGING ---------------------------------------------------------------
    # check if file already exists
    # Update input dictionary
    fig_dir = f"{fig_dir}/{fig_dic['dir']}"
    filepath = f"{fig_dir}/{fig_dic['var']}.png"
    if os.path.isfile(filepath) and not overwrite:
        print(f"\t\t - File already exists: {filepath}, skipping it")
        return

    # Load the data
    gdf = df_in.copy()

    # Make sure gdf is a GeoDataFrame
    gdf = gpd.GeoDataFrame(gdf, geometry="geometry")

    # Unique years to create subplots for
    unique_years = sorted(gdf["year"].unique())
    n_years = len(unique_years)

    # Use some nice font
    plt.rcParams["font.sans-serif"] = "DejaVu Sans"

    # Set up figure and GridSpec
    n_cols = int(np.ceil(n_years / 2))  # Make sure to fit on two rows
    fig = plt.figure(figsize=(15, 8))

    # Allocate the last column for the colorbar and use 2x2 grid for the rest
    gs = gridspec.GridSpec(
        2,
        n_cols + 1,
        height_ratios=[1, 1],
        width_ratios=np.repeat(1, n_cols).tolist() + [0.025],
    )

    # ! Color normalization and colormap ------------------------------------------
    if fig_dic["default_cbar"]:
        # Default
        
        # Taking 95 percentile to avoid outliers dominating coloring 
        # data_max = gdf[fig_dic["var"]].max()
        data_max = np.percentile(gdf[fig_dic["var"]].dropna(), 95)
        data_min = 0
        
        cbar_extend = "max"
        norm = colors.Normalize(vmin=data_min, vmax=data_max)
        sm = plt.cm.ScalarMappable(cmap=fig_dic["cmap"], norm=norm)

    else:
        # Taking 5 and 95 percentile to avoid outliers dominating coloring
        # data_min = gdf[fig_dic["var"]].min()
        # data_max = gdf[fig_dic["var"]].max()
        data_min = np.percentile(gdf[fig_dic["var"]].dropna(), 1)
        data_max = np.percentile(gdf[fig_dic["var"]].dropna(), 99)

        abs_max = max(abs(data_max), abs(data_min))
        # norm = colors.Normalize(vmin=-abs_max, vmax=abs_max)
        cbar_extend = "both"
        norm = colors.TwoSlopeNorm(vmin=-abs_max, vcenter=0, vmax=abs_max)
        sm = plt.cm.ScalarMappable(cmap=fig_dic["cmap"], norm=norm)

    # ! Iterate over the years and create a subplot for each -----------------------
    for i, year in enumerate(unique_years):
        ax = fig.add_subplot(gs[i // n_cols, i % n_cols])

        # Filter the data for the year and plot
        data_for_year = gdf[gdf["year"] == year]
        # Plot it
        plot = data_for_year.plot(
            column=fig_dic["var"],
            edgecolor="face",
            linewidth=0.5,
            ax=ax,
            cmap=fig_dic["cmap"],
            norm=norm,
            missing_kwds={
                "color": "lightgrey",
                "edgecolor": "lightgrey",
                "linewidth": 0.5,
            },
        )

        # Add countour of France
        sp_france.plot(ax=ax, color="none", edgecolor="black", linewidth=0.5)

        # Remove axis
        ax.set_axis_off()

        # Add year as text below the map
        ax.text(
            0.5, 0, str(year), transform=ax.transAxes, ha="center", fontweight="bold"
        )

    # ! Add colorbar --------------------------------------------------------------
    # Create a colorbar in the space of the last column of the first row
    # Span both rows in the last column for the colorbar
    cbar_ax = fig.add_subplot(gs[0:2, n_cols])
    cbar = fig.colorbar(sm, cax=cbar_ax, extend = cbar_extend)
    cbar.set_label(fig_dic["legend"])

    # ! Finish up -----------------------------------------------------------------
    # Adjust layout to accommodate the main title and subplots
    plt.tight_layout(rect=[0, 0, 1, 1])

    # After creating your subplots and before showing or saving the figure
    fig.suptitle(fig_dic["main"], fontsize=16, fontweight="bold", position=(0.5, 1.05))

    # Show/save the figure
    # plt.show()
    
    os.makedirs(fig_dir, exist_ok=True)
    plt.savefig(filepath, bbox_inches="tight", pad_inches=0.1, dpi=300)
    plt.close()
    # print(f"\t\t - Map saved to: {filepath}")
    
    # ! Also save plotted data as feather for later checks -----------------------------
    # CSV is too slow, so saving as feather but limits quicks checks...
    # gdf.to_feather(f"{fig_dir}/{fig_dic['var']}.feather", index=False)

# ------------------------------------------------------------------------------------------
def get_shp_of_region(region):
    # Get corresponding shapefile
    if region == "cty":
        sp = gpd.read_file(here("data/raw/maps/france_geojson/cty.geojson"))
    if region == "reg":
        sp = gpd.read_file(here("data/raw/maps/france_geojson/reg.geojson"))
    if region == "dep":
        sp = gpd.read_file(here("data/raw/maps/france_geojson/dep.geojson"))
    if region == "gre":
        sp = gpd.read_file(here("data/raw/maps/france_geojson/gre.geojson"))
    if region == "ser":
        sp = gpd.read_file(here("data/raw/maps/france_geojson/ser.geojson"))
    if region == "hex":
        sp = load_hexmap()

    sp["year"] = 2010

    for year in range(2011, 2017):
        sp_i = sp.copy()
        sp_i["year"] = year
        sp = pd.concat([sp, sp_i])

    sp = sp.drop_duplicates().reset_index(drop=True)
    return sp

def ___RASTER___():
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
    
    """
    Extracts raster values from a TIFF file at specified latitude and longitude coordinates.

    Args:
        tiff_file (str): The path to the TIFF file.
        variable_name (str): The name of the variable being extracted.
        latitudes (list): A list of latitude coordinates.
        longitudes (list): A list of longitude coordinates.
        progress_bar (bool, optional): Whether to display a progress bar during extraction. Defaults to False.
        expected_crs (str, optional): The expected Coordinate Reference System (CRS) of the input coordinates. Defaults to None.

    Returns:
        pandas.DataFrame: A DataFrame containing the extracted values, latitudes, and longitudes.
    """
    
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

#-----------------------------------------------------------------------------------
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

def parallel_hansen2013_extraction(
        group_in, 
        verbose=False,
        ):

    # Extract values from raster
    df_tc = extract_raster_values(
        here(f"data/raw/hansen2013_treecover_france/treecover_merged_in_python.tif"),
        "treecover",
        group_in["y"],
        group_in["x"],
        progress_bar=verbose,
        expected_crs="4326",
    )
    
    # Extract values from raster
    df_ly = extract_raster_values(
        here(f"data/raw/hansen2013_treecover_france/lossyear_merged_in_python.tif"),
        "lossyear",
        group_in["y"],
        group_in["x"],
        progress_bar=verbose,
        expected_crs="4326",
    )
    
    # Merge them
    df_merged = pd.merge(df_tc, df_ly)
    
    # Rename columns, merge and return
    df_merged = df_merged.rename(columns={"Latitude": "y", "Longitude": "x"})
    
    # Merge again
    df_merged = df_merged.merge(group_in, on=["y", "x"], how="left")
    return df_merged


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
                var_name = my_var + "_" + my_fct + "_in_" + my_season
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


def safran_extract_data_per_site(sites_in, nc_filepath, timestep, verbose=False):
    # Open netcdf dataset
    ds_org = xr.open_dataset(nc_filepath, engine="netcdf4")
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
    if verbose:
        print(" - Attaching new variables...")
    # - Total Precipitation
    ds_org["Precip"] = ds_org["Rainf"] + ds_org["Snowf"]
    # - Saturation Vapor Pressure
    ds_org["SVP"] = 0.6108 * np.exp((17.27 * ds_org["Tair"]) / (ds_org["Tair"] + 237.3))
    # - Actual Vapor Pressure
    ds_org["AVP"] = (ds_org["Qair"] * ds_org["PSurf"]) / (
        0.622 + 0.378 * ds_org["Qair"]
    )
    # - Vapor Pressure Deficit
    ds_org["VPD"] = ds_org["SVP"] - ds_org["AVP"]

    # Aggregate to timestep of interest
    # Ignore RuntimeWarning
    warnings.filterwarnings("ignore", category=RuntimeWarning)
    if verbose:
        print(f" - Aggregating data to {timestep}...")
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
        .groupby(f"tstep.{timestep}")
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
        .groupby(f"tstep.{timestep}")
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

    # Reset warnings
    warnings.resetwarnings()

    # Loop over coordinates
    if verbose:
        print(f" - Looping over {sites_in.shape[0]} sites...")

    df_final = pd.DataFrame()
    for i in tqdm(range(sites_in.shape[0]), disable=not verbose):
        # Get closest point
        lat = sites_in["y"].iloc[i]
        lon = sites_in["x"].iloc[i]
        latloc, lonloc = safran_get_closest_point(ds_org, lat, lon)

        # Extract data
        # - Mean Values
        df_mean = pd.DataFrame()
        for variable in list(ds_means.data_vars):
            df_i = safran_extract_all_timesteps(
                ds_means[variable],
                variable + "_mean",
                timestep,
                latloc,
                lonloc,
                verbose=False,
            )

            if df_mean.empty:
                df_mean = df_i.copy()
            else:
                df_mean = df_mean.merge(df_i, on=timestep, how="outer")

        # - Min Values
        df_min = pd.DataFrame()
        for variable in list(ds_min.data_vars):
            df_i = safran_extract_all_timesteps(
                ds_min[variable],
                variable + "_min",
                timestep,
                latloc,
                lonloc,
                verbose=False,
            )

            if df_min.empty:
                df_min = df_i.copy()
            else:
                df_min = df_min.merge(df_i, on=timestep, how="outer")

        # - Max Values
        df_max = pd.DataFrame()
        for variable in list(ds_max.data_vars):
            df_i = safran_extract_all_timesteps(
                ds_max[variable],
                variable + "_max",
                timestep,
                latloc,
                lonloc,
                verbose=False,
            )

            if df_max.empty:
                df_max = df_i.copy()
            else:
                df_max = df_max.merge(df_i, on=timestep, how="outer")

        # - Sum Values
        df_sum = pd.DataFrame()
        for variable in list(ds_sum.data_vars):
            df_i = safran_extract_all_timesteps(
                ds_sum[variable],
                variable + "_sum",
                timestep,
                latloc,
                lonloc,
                verbose=False,
            )

            if df_sum.empty:
                df_sum = df_i.copy()
            else:
                df_sum = df_sum.merge(df_i, on=timestep, how="outer")

        # Merge all variables of that site
        df_min = df_min.reset_index(drop=True)
        df_max = df_max.reset_index(drop=True)
        df_sum = df_sum.reset_index(drop=True)

        df_site = pd.concat(
            [
                df_mean,
                df_min.drop(timestep, axis=1),
                df_max.drop(timestep, axis=1),
                df_sum.drop(timestep, axis=1),
            ],
            axis=1,
        )
        # Add site id
        df_site["idp"] = sites_in["idp"].iloc[i]

        # Add to df_final
        if df_final.empty:
            df_final = df_site.copy()
        else:
            df_final = pd.concat([df_final, df_site], axis=0)
    # End of loop over sites

    # Move idp to first column
    df_final.insert(0, "idp", df_final.pop("idp"))

    # Reset index
    df_final = df_final.reset_index(drop=True)

    # Close all datasets
    ds_org.close()
    ds_means.close()
    ds_min.close()
    ds_max.close()
    ds_sum.close()

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
