import pandas as pd
import numpy as np
import os


def calculate_growth_mortality(df_in, verbose=False):
    # nesting_vars = df_in.drop(columns=["data"]).columns
    site_id = df_in["idp"].unique()[0]
    if verbose:
        print(
            f"> calculate_growth_mortality():\n  Calculating growth and mortality for site {site_id}."
        )

    n_ini = df_in.shape[0]
    n_sur = df_in[df_in["tree_state_change"] == "alive_alive"].shape[0]
    n_fin = df_in.query(
        "tree_state_change == 'alive_alive' | tree_state_change == 'new_alive'"
    ).shape[0]

    ba_at_v1_of_alive_trees = df_in["ba_1"].sum()
    ba_at_v2_of_alive_trees = df_in.query(
        "tree_state_change == 'alive_alive' | tree_state_change == 'new_alive'"
    )["ba_2"].sum()
    ba_at_v1_of_survivors = df_in.query("tree_state_change == 'alive_alive'")[
        "ba_1"
    ].sum()
    ba_at_v2_of_survivors = df_in.query("tree_state_change == 'alive_alive'")[
        "ba_2"
    ].sum()
    ba_at_v2_of_dead = df_in.query("tree_state_change == 'alive_dead'")["ba_2"].sum()

    if n_sur == 0:
        n_mor_yr_esq = np.nan
        n_mor_yr = np.nan
        n_rec_yr = np.nan
        ba_loss_yr = np.nan
        ba_gain_yr = np.nan
        ba_ingr_yr = np.nan
        ba_growth_abs = np.nan
        ba_growth_rate = np.nan
        ba_loss_abs = np.nan
        ba_loss_rate = np.nan

    else:
        # Mortality following Esquivel et al.
        n_mor_yr_esq = (1 - (n_sur / n_ini) ** (1 / 5)) * 100

        # Mortality and Recruitment following Hoshino et al.
        n_mor_yr = np.log(n_ini / n_sur) / 5 * 100
        n_rec_yr = np.log(n_fin / n_sur) / 5 * 100

        # Loss, gain, ingrowth following Hoshino et al.
        ba_loss_yr = np.log(ba_at_v1_of_alive_trees / ba_at_v1_of_survivors) / 5 * 100
        ba_gain_yr = np.log(ba_at_v2_of_alive_trees / ba_at_v1_of_survivors) / 5 * 100
        ba_ingr_yr = np.log(ba_at_v2_of_survivors / ba_at_v1_of_survivors) / 5 * 100

        # Isolated growth of basal area
        ba_growth_abs = (ba_at_v2_of_alive_trees - ba_at_v1_of_survivors) / 5
        ba_growth_rate = ba_growth_abs / ba_at_v1_of_survivors * 100

        # Isolated loss of basal area
        ba_loss_abs = ba_at_v2_of_dead / 5
        ba_loss_rate = ba_loss_abs / (ba_at_v2_of_dead + ba_at_v2_of_survivors) * 100

    # Change in alive basal area
    ba_change_abs = ba_at_v2_of_alive_trees - ba_at_v1_of_alive_trees / 5
    ba_change_rel = ba_change_abs / ba_at_v1_of_alive_trees / 5

    if "idp" in df_in.columns:
        if verbose:
            print(
                f"> calculate_growth_mortality():\n  idp detected in columns, so dividing absolute rates by number of plots."
            )

        n_plots = df_in["idp"].nunique()
        ba_growth_abs = ba_growth_abs / n_plots
        ba_loss_abs = ba_loss_abs / n_plots
        ba_change_abs = ba_change_abs / n_plots
    else:
        n_plots = 1

    df_out = pd.DataFrame(
        {
            "idp": site_id,
            "n_ini": n_ini,
            "n_sur": n_sur,
            "n_fin": n_fin,
            "ba_at_v1_of_alive_trees": ba_at_v1_of_alive_trees,
            "ba_at_v2_of_alive_trees": ba_at_v2_of_alive_trees,
            "ba_at_v1_of_survivors": ba_at_v1_of_survivors,
            "ba_at_v2_of_survivors": ba_at_v2_of_survivors,
            "ba_at_v2_of_dead": ba_at_v2_of_dead,
            "ba_loss_yr": ba_loss_yr,
            "ba_gain_yr": ba_gain_yr,
            "ba_ingr_yr": ba_ingr_yr,
            "ba_growth_abs": ba_growth_abs,
            "ba_growth_rate": ba_growth_rate,
            "ba_loss_abs": ba_loss_abs,
            "ba_loss_rate": ba_loss_rate,
            "ba_change_abs": ba_change_abs,
            "ba_change_rel": ba_change_rel,
        },
        index=[0],
    )

    return df_out