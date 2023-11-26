import pandas as pd
import numpy as np
import os


def calculate_growth_mortality(df_in, verbose=False, divide_by_nplots=False, grouping_variable="idp"):

    if (divide_by_nplots) & (grouping_variable == "idp"):
        os.error("Cannot divide by number of plots if grouping variable is idp.")
    
    my_group = df_in[grouping_variable].unique()[0]
    
    if verbose:
        print(
            f"> calculate_growth_mortality():\n  Calculating growth and mortality for {grouping_variable}: {my_group}."
        )

    # -------------------------------------------------------------------------
    # Stem-based metrics
    n_ini = df_in.shape[0]
    n_sur = df_in[df_in["tree_state_change"] == "alive_alive"].shape[0]
    n_fin = df_in.query("tree_state_change == 'alive_alive' | tree_state_change == 'new_alive'").shape[0]
    n_rec = df_in[df_in["tree_state_change"] == "new_alive"].shape[0]
    n_die = df_in[df_in["tree_state_change"] == "alive_dead"].shape[0]

    # -------------------------------------------------------------------------
    # Basal area-based metrics
    ba_at_v1_of_alive_trees = df_in["ba_1"].sum()
    ba_at_v2_of_alive_trees = df_in.query("tree_state_change == 'alive_alive' | tree_state_change == 'new_alive'")["ba_2"].sum()

    ba_at_v1_of_survivors = df_in.query("tree_state_change == 'alive_alive'")["ba_1"].sum()
    ba_at_v2_of_survivors = df_in.query("tree_state_change == 'alive_alive'")["ba_2"].sum()
    
    ba_at_v1_of_dead     = df_in.query("tree_state_change == 'alive_dead'")["ba_1"].sum()
    ba_at_v2_of_dead     = df_in.query("tree_state_change == 'alive_dead'")["ba_2"].sum()
    ba_at_v2_of_recruits = df_in.query("tree_state_change == 'new_alive'")["ba_2"].sum()

    # Mortality following Esquivel et al.
    if  n_ini == 0:
        mort_stems_prc_yr_esq = np.nan
    else:
        mort_stems_prc_yr_esq = (1 - (n_sur / n_ini) ** (1 / 5)) * 100

    # Mortality and Recruitment following Hoshino et al.
    if n_sur == 0:
        mort_stems_prc_yr_hoshino = np.nan
        rec_stems_prc_yr_hoshino  = np.nan
    else:
        mort_stems_prc_yr_hoshino = np.log(n_ini / n_sur) / 5 * 100
        rec_stems_prc_yr_hoshino  = np.log(n_fin / n_sur) / 5 * 100

    # Loss, gain, ingrowth following Hoshino et al.
    if ba_at_v1_of_survivors == 0:
        mort_ba_prc_yr_hoshino       = np.nan
        tot_growth_ba_prc_yr_hoshino = np.nan
        sur_growth_ba_prc_yr_hoshino = np.nan
    else:        
        mort_ba_prc_yr_hoshino       = np.log(ba_at_v1_of_alive_trees / ba_at_v1_of_survivors) / 5 * 100
        tot_growth_ba_prc_yr_hoshino = np.log(ba_at_v2_of_alive_trees / ba_at_v1_of_survivors) / 5 * 100
        sur_growth_ba_prc_yr_hoshino = np.log(ba_at_v2_of_survivors   / ba_at_v1_of_survivors) / 5 * 100

    # Absolute changes
    tot_growth_ba_yr  = (ba_at_v2_of_alive_trees - ba_at_v1_of_survivors) / 5
    sur_growth_ba_yr  = (ba_at_v2_of_survivors - ba_at_v1_of_survivors) / 5
    mort_ba_yr_v1     = ba_at_v1_of_dead / 5
    mort_ba_yr_v2     = ba_at_v2_of_dead / 5
    
    # Relative changes
    if ba_at_v1_of_survivors == 0:
        tot_growth_ba_prc_yr = np.nan
        sur_growth_ba_prc_yr = np.nan
    else:  
        tot_growth_ba_prc_yr    = tot_growth_ba_yr / ba_at_v1_of_survivors * 100
        sur_growth_ba_prc_yr    = sur_growth_ba_yr / ba_at_v1_of_survivors * 100
        
    if ba_at_v1_of_alive_trees == 0:
        mort_ba_prc_yr_v1 = np.nan
    else:
        mort_ba_prc_yr_v1 = mort_ba_yr_v1 / ba_at_v1_of_alive_trees * 100
    
    if (ba_at_v2_of_dead + ba_at_v2_of_alive_trees) == 0:
        mort_ba_prc_yr_v2 = np.nan
    else:
        mort_ba_prc_yr_v2 = mort_ba_yr_v2 / (ba_at_v2_of_dead + ba_at_v2_of_alive_trees) * 100

    # -------------------------------------------------------------------------
    # Divide by number of plots if required
    if divide_by_nplots:
        if verbose:
            print(
                f"\n Dividing all metrics by number of plots per grouping variable: {grouping_variable} so that they are in terms of per plot."
            )

        n_plots = df_in["idp"].nunique()
        n_ini = n_ini / n_plots
        n_sur = n_sur / n_plots
        n_fin = n_fin / n_plots
        n_rec = n_rec / n_plots
        n_die = n_die / n_plots
        ba_at_v1_of_alive_trees = ba_at_v1_of_alive_trees / n_plots
        ba_at_v2_of_alive_trees = ba_at_v2_of_alive_trees / n_plots
        ba_at_v1_of_survivors = ba_at_v1_of_survivors / n_plots
        ba_at_v2_of_survivors = ba_at_v2_of_survivors / n_plots
        ba_at_v2_of_dead = ba_at_v2_of_dead / n_plots
        ba_at_v2_of_recruits = ba_at_v2_of_recruits / n_plots
        mort_stems_prc_yr_esq = mort_stems_prc_yr_esq / n_plots
        mort_stems_prc_yr_hoshino = mort_stems_prc_yr_hoshino / n_plots
        rec_stems_prc_yr_hoshino = rec_stems_prc_yr_hoshino / n_plots
        mort_ba_prc_yr_hoshino = mort_ba_prc_yr_hoshino / n_plots
        tot_growth_ba_prc_yr_hoshino = tot_growth_ba_prc_yr_hoshino / n_plots
        sur_growth_ba_prc_yr_hoshino = sur_growth_ba_prc_yr_hoshino / n_plots
        tot_growth_ba_yr = tot_growth_ba_yr / n_plots
        sur_growth_ba_yr = sur_growth_ba_yr / n_plots
        mort_ba_yr_v1 = mort_ba_yr_v1 / n_plots
        mort_ba_yr_v2 = mort_ba_yr_v2 / n_plots
        tot_growth_ba_prc_yr = tot_growth_ba_prc_yr / n_plots
        sur_growth_ba_prc_yr = sur_growth_ba_prc_yr / n_plots
        mort_ba_prc_yr_v1 = mort_ba_prc_yr_v1 / n_plots
        mort_ba_prc_yr_v2 = mort_ba_prc_yr_v2 / n_plots  
        
    else:
        n_plots = 1

    df_out = pd.DataFrame(
        {
            grouping_variable: my_group,
            "n_plots": n_plots,
            "n_ini": n_ini,
            "n_sur": n_sur,
            "n_fin": n_fin,
            "n_rec": n_rec,
            "n_die": n_die,
            "ba_at_v1_of_alive_trees": ba_at_v1_of_alive_trees,
            "ba_at_v2_of_alive_trees": ba_at_v2_of_alive_trees,
            "ba_at_v1_of_survivors": ba_at_v1_of_survivors,
            "ba_at_v2_of_survivors": ba_at_v2_of_survivors,
            "ba_at_v1_of_dead": ba_at_v1_of_dead,
            "ba_at_v2_of_dead": ba_at_v2_of_dead,
            "ba_at_v2_of_recruits": ba_at_v2_of_recruits,
            "mort_stems_prc_yr_esq": mort_stems_prc_yr_esq,
            "mort_stems_prc_yr_hoshino": mort_stems_prc_yr_hoshino,
            "rec_stems_prc_yr_hoshino": rec_stems_prc_yr_hoshino,
            "mort_ba_prc_yr_hoshino": mort_ba_prc_yr_hoshino,
            "tot_growth_ba_prc_yr_hoshino": tot_growth_ba_prc_yr_hoshino,
            "sur_growth_ba_prc_yr_hoshino": sur_growth_ba_prc_yr_hoshino,
            "tot_growth_ba_yr": tot_growth_ba_yr,
            "sur_growth_ba_yr": sur_growth_ba_yr,
            "mort_ba_yr_v1": mort_ba_yr_v1,
            "mort_ba_yr_v2": mort_ba_yr_v2,
            "tot_growth_ba_prc_yr": tot_growth_ba_prc_yr,
            "sur_growth_ba_prc_yr": sur_growth_ba_prc_yr,
            "mort_ba_prc_yr_v1": mort_ba_prc_yr_v1,
            "mort_ba_prc_yr_v2": mort_ba_prc_yr_v2,
        },
        index=[0],
    )

    return df_out
