
# Data wrangling
from matplotlib.pylab import f
import pandas as pd
import numpy as np
import random

# Data visualisation
from ydata_profiling import ProfileReport
import matplotlib.pyplot as plt
import seaborn as sns

# Machine learning
from sklearn.ensemble import RandomForestRegressor, RandomForestClassifier
from sklearn.model_selection import train_test_split, GridSearchCV, StratifiedKFold
from sklearn.metrics import r2_score, mean_squared_error, mean_absolute_error
from sklearn.metrics import confusion_matrix, accuracy_score, classification_report
from sklearn.preprocessing import OneHotEncoder
from sklearn.impute import KNNImputer
from sklearn.inspection import PartialDependenceDisplay

# My functions
import sys
sys.path.insert(0, "../../src")
from run_mp import *
from utilities import *
from random_forest_utils import *

# Other
from os import error
import datetime
import re

# -----------------------------------------------------------------------------------------------

def create_new_run_folder(folder_suffix=None):
    # Get today's date
    today = datetime.date.today().strftime("%Y-%m-%d")

    # Create the subdirectory in "model_runs" with today's date as the name
    subdirectory = os.path.join("model_runs", today)
    os.makedirs(subdirectory, exist_ok=True)

    # Set folder pattern in daily folder
    folder_pattern = "run_"

    # Filter subdirectory to regex match the folder_pattern (omits other files and folders)
    all_folders = [
        folder
        for folder in os.listdir(subdirectory)
        if re.match(folder_pattern, folder)
    ]

    # Count the number of folders in the subdirectory
    num_folders = len(all_folders)

    # print(num_folders, all_folders)

    # Create a new folder with the name "run_n" where n is the number of folders + 1
    if num_folders < 9:
        folder_nr = f"0{num_folders + 1}"
    else:
        folder_nr = num_folders + 1

    new_folder = os.path.join(subdirectory, f"{folder_pattern}{folder_nr}")

    if folder_suffix:
        new_folder += f"_{folder_suffix}"

    os.makedirs(new_folder)

    return new_folder

# -----------------------------------------------------------------------------------------------
def get_current_folder():
    # Get today's date
    today = datetime.date.today().strftime("%Y-%m-%d")

    # Create the subdirectory in "model_runs" with today's date as the name
    subdirectory = os.path.join("model_runs", today)

    # Set folder pattern in daily folder
    folder_pattern = "run_"

    # Filter subdirectory to regex match the folder_pattern (omits other files and folders)
    all_folders = [
        folder
        for folder in os.listdir(subdirectory)
        if re.match(folder_pattern, folder)
    ]

    # Since folders are sorted by run number, the last in the list is the newest
    # print(sorted(all_folders))
    current_folder = sorted(all_folders)[-1]
    return current_folder
# -----------------------------------------------------------------------------------------------
def plot_grid_search_results(grid, rnd_or_psc=None, save_directory=None):
    """
    Params:
        grid: A trained GridSearchCV object.
    """
    ## Results from grid search
    results = grid.cv_results_
    means_test = results["mean_test_score"]
    stds_test = results["std_test_score"]
    means_train = results["mean_train_score"]
    stds_train = results["std_train_score"]

    ## Getting indexes of values per hyper-parameter
    masks = []
    masks_names = list(grid.best_params_.keys())
    for p_k, p_v in grid.best_params_.items():
        masks.append(list(results["param_" + p_k].data == p_v))

    params = grid.param_grid

    ## Ploting results
    fig, ax = plt.subplots(1, len(params), sharex="none", sharey="all", figsize=(20, 5))
    fig.suptitle("Score per parameter")
    fig.text(0.04, 0.5, "MEAN SCORE", va="center", rotation="vertical")
    pram_preformace_in_best = {}
    for i, p in enumerate(masks_names):
        m = np.stack(masks[:i] + masks[i + 1 :])
        pram_preformace_in_best
        best_parms_mask = m.all(axis=0)
        best_index = np.where(best_parms_mask)[0]
        x = np.array(params[p])
        y_1 = np.array(means_test[best_index])
        e_1 = np.array(stds_test[best_index])
        y_2 = np.array(means_train[best_index])
        e_2 = np.array(stds_train[best_index])
        ax[i].errorbar(x, y_1, e_1, linestyle="--", marker="o", label="test")
        ax[i].errorbar(x, y_2, e_2, linestyle="-", marker="^", label="train")
        ax[i].set_xlabel(p.upper())

    plt.legend()
    plt.show()
    if save_directory is not None:
        if rnd_or_psc is None:
            raise ValueError("rnd_or_psc must be specified")
        else:
            fig.savefig(f"{save_directory}/fig_grid_search_{rnd_or_psc}.png")
    # return fig
# -----------------------------------------------------------------------------------------------
def show_top_predictors(
    X_train=None,
    vars_to_ohe=None,
    rf_model=None,
    with_aggregation=False,
    n_predictors=20,
    verbose=False,
    current_dir=None,
):
    # Plot the variable importance
    importances = rf_model.feature_importances_
    indices = np.argsort(importances)[::-1]

    df_featimp = pd.DataFrame(
        {
            "Feature": X_train.columns[indices],
            "Importance": importances[indices],
        }
    )

    if verbose:
        print("Original size of df_featimp: ", df_featimp.shape)

    if with_aggregation:
        # For features matching the string in vars_to_ohe_red, sum up their importances and set name to vars_to_ohe_red
        # Make sure aggregation procedure is saved to file for checking later on:

        if vars_to_ohe is None:
            raise ValueError("vars_to_ohe must be specified for aggregation!")
        
        if verbose:
            display("Aggregating variables...")

        rows_to_drop = []
        rows_to_append = []
        text_to_save = []
        agg_dict = {}

        # vars_to_ohe_red = [var for var in vars_to_ohe if var in X_train.columns]

        # for var in vars_to_ohe_red:
        #     n_vars = 0
        #     feat_sum = 0
        #     merged_vars = []
        #     for i in range(len(df_featimp)):
        #         if var in df_featimp.loc[i, "Feature"]:
        #             merged_vars.append(df_featimp.loc[i, "Feature"])
        #             feat_sum += df_featimp.loc[i, "Importance"]
        #             n_vars += 1
        #             rows_to_drop.append(i)

        for var in vars_to_ohe:
            n_vars = 0
            feat_sum = 0
            merged_vars = []
            pattern = r"^" + var + r"_.*"

            for i in range(len(df_featimp)):
                if re.match(pattern, df_featimp.loc[i, "Feature"]):
                    merged_vars.append(df_featimp.loc[i, "Feature"])
                    feat_sum += df_featimp.loc[i, "Importance"]
                    n_vars += 1
                    rows_to_drop.append(i)

            # Attach to rows_to_append
            rows_to_append.append({"Feature": var, "Importance": feat_sum})

            # Print aggregation information
            # print(f"Merged {n_vars} vars into {var} containing: {merged_vars}")

            # Save aggregation to a dictionary
            agg_dict[var] = merged_vars

            # Save information to file
            if verbose:
                text_to_save = text_to_save + [
                    f"Merged {n_vars} vars into {var} containing:\n {merged_vars} \n\n"
                ]

        # Drop the rows that were merged
        df_featimp = df_featimp.drop(rows_to_drop)
        df_featimp = pd.concat(
            [df_featimp, pd.DataFrame(rows_to_append)], ignore_index=True
        )
        df_featimp = df_featimp.sort_values(by="Importance", ascending=False)

        if verbose:
            print("df_featimp after merging: ", df_featimp.shape)

            # Write to file
            file_path = f"{current_dir}/vip_aggregation_of_ohe_into_their_originals.txt"
            with open(file_path, "w") as file:
                for item in text_to_save:
                    file.write(f"{item}\n\n")
    else:
        agg_dict = None

    top_n = df_featimp.head(n_predictors)

    if verbose:
        # Show top n predictors table
        display(top_n)

    # Plot the variable importance
    sns_plot = sns.barplot(x="Importance", y="Feature", data=top_n, color="r")
    plt.tight_layout()

    # Save the barplot as an image file
    sns_plot.figure.savefig(
        f"{current_dir}/vip_plot_aggregated-{with_aggregation}.png"
    )

    # Save the dataframe as a tab-separated file
    df_featimp.to_csv(f"{current_dir}/vip_table_aggregated-{with_aggregation}.csv")
    
    if verbose:
        plt.show()
    plt.close()

    # Return aggregation dictionary if needed
    return df_featimp, agg_dict
# -----------------------------------------------------------------------------------------------
def assessing_top_predictors(
    rf_in,
    ignore_these,
    X_train_in,
    dict_ohe_in,
    with_aggregation=False,
    n_predictors=20,
    verbose=True,
    save_directory=None,
):
    # Update n_predictors based on number of variables in X_train_in
    n_predictors = min(n_predictors, len(X_train_in.columns))

    # Get importances
    importances = rf_in.feature_importances_
    std = np.std([tree.feature_importances_ for tree in rf_in.estimators_], axis=0)
    indices = np.argsort(importances)[::-1]

    df_featimp_org = pd.DataFrame(
        {
            "Feature": X_train_in.columns[indices],
            "Importance": importances[indices],
            "Std": std[indices],
        }
    )

    df_featimp_agg = pd.DataFrame(
        {
            "Feature": [],
            "Importance": [],
            "Std": [],
        }
    )

    # Ugly quick fix but works...
    if not with_aggregation:
        reduced_var_ohe_dict = dict_ohe_in

    if with_aggregation:
        # Reduce dictionary to hold only keys for which there is a value that matches the inputed X_train_in
        reduced_var_ohe_dict = {
            key: value
            for key, value in dict_ohe_in.items()
            if any(col in X_train_in.columns for col in value)
        }

        # Remove variables to ignore from dictionary
        for var in ignore_these:
            if var in reduced_var_ohe_dict.keys():
                del reduced_var_ohe_dict[var]

        # Loop through all keys in the dictionary
        for key in reduced_var_ohe_dict.keys():
            importances_per_key = []
            stdevs_per_key = []

            # Loop through all variables in the key
            for var in reduced_var_ohe_dict[key]:
                # Loop through all variables in the original featimp df
                for i in range(len(df_featimp_org)):
                    # Check if the row in the featimp corresponds to value of the key
                    # If so, gather all variables for that key and aggregate them.
                    if df_featimp_org["Feature"].iloc[i] == var:
                        importances_per_key.append(df_featimp_org["Importance"].iloc[i])
                        stdevs_per_key.append(df_featimp_org["Importance"].iloc[i])

                n_vars_per_key = len(reduced_var_ohe_dict[key])
                if n_vars_per_key > 1:
                    importance = np.sum(importances_per_key)
                    stdev = sum([x**2 for x in stdevs_per_key])
                else:
                    importance = importances_per_key[0]
                    stdev = stdevs_per_key[0]

            new_row = pd.DataFrame(
                {
                    "Feature": [key],
                    "Importance": [importance],
                    "Std": [stdev],
                    "Importance_per_key": [importances_per_key],
                    "Vars_per_key": [n_vars_per_key],
                    "Vars_in_key": [list(reduced_var_ohe_dict[key])],
                }
            )

            df_featimp_agg = pd.concat([df_featimp_agg, new_row], axis=0)
            df_featimp_final = df_featimp_agg.sort_values(
                by="Importance", ascending=False
            ).reset_index(drop=True)

    else:
        df_featimp_final = (
            df_featimp_org.copy()
            .sort_values(by="Importance", ascending=False)
            .reset_index(drop=True)
        )

    # --------------------------------------------------------
    if verbose:
        # Show top n predictors table
        top_n = df_featimp_final.head(n_predictors)
        if with_aggregation:
            my_title = f"Top {n_predictors} predictors with aggregation"
        else:
            my_title = f"Top {n_predictors} predictors without aggregation"

        # Plot the variable importance
        sns_plot = sns.barplot(x="Importance", y="Feature", data=top_n, color="r")
        sns_plot.set_title(my_title)
        plt.tight_layout()
        if save_directory is not None:
            if with_aggregation:
                sns_plot.figure.savefig(f"{save_directory}/fig_vip_plot_aggregated.png")
            else:
                sns_plot.figure.savefig(f"{save_directory}/fig_vip_plot_not_aggregated.png")

        # Save the barplot as an image file
        # sns_plot.figure.savefig(
        #     f"{current_dir}/vip_plot_aggregated-{with_aggregation}.png"
        # )

        # Save the dataframe as a tab-separated file
        # df_featimp.to_csv(f"{current_dir}/vip_table_aggregated-{with_aggregation}.csv")

        print(
            f"assessing_top_predictors()...",
            f"\n - Number of columns in X_train_in is equal to number of rows in df_featimp_org: {len(X_train_in.columns) == df_featimp_org.shape[0]}",
            f"\n - Number of vars in training set is equal to number of rows in df_featimp_agg: {len(reduced_var_ohe_dict) == df_featimp_agg.shape[0]}",
            f"\n - Size of original df_featimp_org: {df_featimp_org.shape}",
            f"\n - Size of aggregated df_featimp_agg: {df_featimp_agg.shape}",
        )

    return df_featimp_final
# -----------------------------------------------------------------------------------------------
def model_evaluation_regression(rf_model, X_train, y_train, X_test, y_test, save_directory=None, verbose=False):
    # Predict on the train and test data
    y_train_pred = rf_model.predict(X_train)
    y_test_pred = rf_model.predict(X_test)
    
    # Calculate the evaluation metrics for train data
    r2_train = r2_score(y_train, y_train_pred)
    rmse_train = np.sqrt(mean_squared_error(y_train, y_train_pred))
    mae_train = mean_absolute_error(y_train, y_train_pred, )

    # Calculate the evaluation metrics for test data
    r2_test = r2_score(y_test, y_test_pred)
    rmse_test = np.sqrt(mean_squared_error(y_test, y_test_pred))
    mae_test = mean_absolute_error(y_test, y_test_pred)
    
    # Print the evaluation metrics for train data
    if verbose:
        print("Train Data:")
        print(" - R2:\t\t ", round(r2_train, 2))
        print(" - RMSE:\t ", round(rmse_train, 2))
        print(" - MAE:\t\t ", round(mae_train, 2))
        print(" - R2 numpy:\t ", round(np.corrcoef(y_train, y_train_pred)[0,1], 2))
        print(" - RMSE numpy:\t ", round(np.sqrt(np.mean((y_train - y_train_pred)**2)), 2))
        print(" - MAE numpy:\t ", round(np.mean(np.abs(y_train - y_train_pred)), 2))

        # Print the evaluation metrics for test data
        print("\nTest Data:")
        print(" - R2:\t\t ", round(r2_test, 2))
        print(" - RMSE:\t ", round(rmse_test, 2))
        print(" - MAE:\t\t ", round(mae_test, 2))
        print(" - R2 numpy:\t ", round(np.corrcoef(y_test, y_test_pred)[0,1], 2))
        print(" - RMSE numpy:\t ", round(np.sqrt(np.mean((y_test - y_test_pred)**2)), 2))
        print(" - MAE numpy:\t ", round(np.mean(np.abs(y_test - y_test_pred)), 2))

    # Set the figure size
    plt.figure(figsize=(12, 6))

    # Plot the predicted versus observed values for train data
    plt.subplot(1, 2, 1)
    sns.regplot(
        x=y_train,
        y=y_train_pred,
        scatter_kws=dict(color="gray", s=10, alpha=0.8),
        line_kws=dict(color="blue"),
    )
    plt.plot(ls="--", c="red")
    plt.xlabel("Observations")
    plt.ylabel("Predictions")
    plt.title(f"Train Data (target: {y_test.name})")

    # Set y and x axis limits based on the maximum value in y_train_pred or y_train
    max_value_train = max(max(y_train_pred), max(y_train))
    plt.ylim(0, max_value_train*1.15)
    plt.xlim(0, max_value_train*1.15)

    # Add a red dotted 1:1 line
    plt.plot([0, max_value_train], [0, max_value_train], ls="--", c="r")

    # Set equal scaling (i.e., 1:1 aspect ratio)
    plt.gca().set_aspect("equal", adjustable="box")

    # Add metrics reporting to the top right corner
    plt.text(
        0.2,
        0.95,
        f"R2: {round(r2_train, 2)}\nRMSE: {round(rmse_train, 2)}\nMAE: {round(mae_train, 2)}",
        horizontalalignment="right",
        verticalalignment="top",
        transform=plt.gca().transAxes,
        bbox=dict(facecolor="white", edgecolor="black", boxstyle="round"),
    )

    # Plot the predicted versus observed values for test data
    plt.subplot(1, 2, 2)
    sns.regplot(
        x=y_test,
        y=y_test_pred,
        scatter_kws=dict(color="gray", s=10, alpha=0.8),
        line_kws=dict(color="blue"),
    )
    plt.plot(ls="--", c="red")
    plt.xlabel("Observations")
    plt.ylabel("Predictions")
    plt.title(f"Test Data (target: {y_train.name})")

    # Set y and x axis limits based on the maximum value in y_test_pred or y_test
    max_value_test = max(max(y_test_pred), max(y_test))
    plt.ylim(0, max_value_test*1.15)
    plt.xlim(0, max_value_test*1.15)

    # Add a red dotted 1:1 line
    plt.plot([0, max_value_test], [0, max_value_test], ls="--", c="r")

    # Set equal scaling (i.e., 1:1 aspect ratio)
    plt.gca().set_aspect("equal", adjustable="box")

    # Add metrics reporting to the top right corner
    plt.text(
        0.2,
        0.95,
        f"R2: {round(r2_test, 2)}\nRMSE: {round(rmse_test, 2)}\nMAE: {round(mae_test, 2)}",
        horizontalalignment="right",
        verticalalignment="top",
        transform=plt.gca().transAxes,
        bbox=dict(facecolor="white", edgecolor="black", boxstyle="round"),
    )

    # Adjust the spacing between subplots
    plt.tight_layout()

    # Save the figure
    if save_directory is not None:
        os.makedirs(save_directory, exist_ok=True)
        plt.savefig(f"{save_directory}/fig_model_evaluation.png")
    
    # Show the figures
    if verbose:
        plt.show()
    plt.close()
# -----------------------------------------------------------------------------------------------
def model_evaluation_classification(rf_model, X_train, y_train, X_test, y_test, save_directory =None):
    # Predict on the train and test data
    y_train_pred = rf_model.predict(X_train)
    y_test_pred = rf_model.predict(X_test)

    # Get unique class labels from the target variable
    unique_labels = np.unique(np.concatenate((y_train, y_test)))

    # Calculate the confusion matrix for train data
    confusion_train = confusion_matrix(y_train, y_train_pred, labels=unique_labels)

    # Calculate the confusion matrix for test data
    confusion_test = confusion_matrix(y_test, y_test_pred, labels=unique_labels)

    # Get accuracy
    accuracy_train = accuracy_score(y_train, y_train_pred)
    accuracy_test = accuracy_score(y_test, y_test_pred)
    accuracy_train_max = extract_highest_accuracy(y_train, y_train_pred)
    accuracy_test_max = extract_highest_accuracy(y_test, y_test_pred)
    accuracy_max = max(accuracy_train_max, accuracy_test_max)
    
    # Print the confusion matrix for train data
    print("Train Data:")
    print(confusion_train)
    print("\nAccuracy:", round(accuracy_train, 2))
    print("\nClassification Report:\n", classification_report(y_train, y_train_pred))

    # Print the confusion matrix for test data
    display("-----------------------")
    print("Test Data:")
    print(confusion_test)
    print("\nAccuracy:", round(accuracy_test, 2))
    print("\nClassification Report:\n", classification_report(y_test, y_test_pred))
    
    # Write the same information to a file
    if save_directory is not None:
        file_path = f"{save_directory}/classification_report.txt"
        with open(file_path, "w") as file:
            file.write(f"Train Data:\n{confusion_train}\n\nAccuracy: {round(accuracy_train, 2)}\n\nClassification Report:\n{classification_report(y_train, y_train_pred)}\n\n")
            file.write(f"Test Data:\n{confusion_test}\n\nAccuracy: {round(accuracy_test, 2)}\n\nClassification Report:\n{classification_report(y_test, y_test_pred)}\n\n")

    # Set the figure size
    plt.figure(figsize=(12, 6))

    # Combine confusion matrices to find common color scale
    combined_confusion = np.maximum(confusion_train, confusion_test)

    # Plot the confusion matrix for train data
    plot_confusion_matrix(confusion_train, unique_labels, title="Train Data", bpmax=accuracy_max, save_directory=save_directory, test_or_train="train")


    # Plot the confusion matrix for test data
    plt.figure(figsize=(12, 6))
    plot_confusion_matrix(confusion_test, unique_labels, title="Test Data", bpmax=accuracy_max, save_directory= save_directory, test_or_train="test")



def plot_confusion_matrix(conf_matrix, class_labels, title, bpmax=None,save_directory=None, test_or_train="none"):
    """
    Plots the confusion matrix and class accuracy for a given set of class labels.

    Parameters:
    conf_matrix (numpy.ndarray): The confusion matrix.
    class_labels (list): The list of class labels.
    title (str): The title of the plot.
    vmax (int, optional): The maximum value for the colorbar. Defaults to None.
    bpmax (int, optional): The maximum value for the y-axis of the bar plot. Defaults to None.

    Returns:
    None
    """
    # Compute accuracy for each class
    class_accuracy = np.diag(conf_matrix) / np.sum(conf_matrix, axis=1)

    # Set up the plot
    plt.subplot(1, 2, 1)
    # Plot heatmap
    sns.heatmap(conf_matrix, annot=True, fmt="d", cmap="Blues", xticklabels=class_labels, yticklabels=class_labels[::-1])
    plt.title(title)
    plt.xlabel("Actual")
    plt.ylabel("Predicted")

    # Plot class accuracy
    plt.subplot(1, 2, 2)
    if bpmax is not None:
        plt.ylim(0, bpmax*1.1)
    plt.axhline(y=0.5, color='grey', linestyle='dotted')
    plt.bar(class_labels, class_accuracy, color="steelblue")
    plt.title("Class Accuracy")
    plt.xlabel("Class")
    plt.ylabel("Accuracy")

    # Adjust the spacing between subplots
    plt.tight_layout()

    # Save the figure
    if save_directory is not None:
        plt.savefig(f"{save_directory}/fig_model_evaluation_{test_or_train}.png")
    
    # Show the figures
    plt.show()

def extract_highest_accuracy(y_true, y_pred):
    # Calculate accuracy for each class
    class_accuracy = np.diag(confusion_matrix(y_true, y_pred)) / np.sum(confusion_matrix(y_true, y_pred), axis=1)

    # Find the highest accuracy
    highest_accuracy = np.max(class_accuracy)

    return highest_accuracy

# -----------------------------------------------------------------------------------------------
def do_ohe(Xy, variables_not_to_ohe=[], verbose=True):
    ohe_these = []

    for var in Xy:
        if Xy[var].dtype == "O":
            ohe_these = ohe_these + [var]

    # Remove variables that should not be one-hot encoded
    ohe_these = [var for var in ohe_these if var not in variables_not_to_ohe]

    # One-hot encoding
    ohe = OneHotEncoder(sparse_output=False, handle_unknown="ignore")

    df_ohe = pd.DataFrame(
        ohe.fit_transform(Xy[ohe_these]),
        columns=ohe.get_feature_names_out(ohe_these),
    )

    # Attach the non-encoded variables
    df_out = pd.concat(
        [
            Xy.drop(columns=ohe_these).reset_index(drop=True),
            df_ohe.reset_index(drop=True),
        ],
        axis=1,
    )

    # Verbose output
    if verbose:
        print(
            f"do_ohe():",
            f"\n - Shape before OHE:\t\t {Xy.shape}",
            f"\n - Shape after OHE:\t\t {df_out.shape}",
            f"\n - Change in Nr. of columns:\t {df_out.shape[1] - Xy.shape[1]} (dropped: {len(ohe_these)}, added: {df_ohe.shape[1]})",
            f"\n - Variables that were ohe:\t {' | '.join(sorted(ohe_these))}",
            f"\n - New variables:\t\t {' | '.join(sorted(df_ohe.columns.to_list()))}",
        )

    return df_out
# -----------------------------------------------------------------------------------------------
def impute_numerical_na(
    Xy_train_in,
    Xy_test_in,
    target_in,
    method="knn",
    n_neighbours=10,
    vars_not_to_impute=[],
    verbose=True,
):
    # Define X for imputation
    X_train_in = Xy_train_in.copy().drop(columns=target_in).reset_index(drop=True)
    X_test_in = Xy_test_in.copy().drop(columns=target_in).reset_index(drop=True)

    # Get numerical variables with NAs (need to check each dataset separately)
    all_numerics = X_train_in.columns[(X_train_in.dtypes != "O")].tolist()

    num_na_train = X_train_in.columns[
        (X_train_in.dtypes != "O") & (X_train_in.isna().any())
    ].tolist()
    num_na_test = X_test_in.columns[
        (X_test_in.dtypes != "O") & (X_test_in.isna().any())
    ].tolist()

    # Remove target from imputation if still in dataset)
    if target_in in num_na_train:
        num_na_train.remove(target_in, errors="ignore")
    if target_in in num_na_test:
        num_na_test.remove(target_in, errors="ignore")

    num_na_train = list(set(num_na_train + num_na_test))

    # Check if target had NA values and inform
    detected_vars = []
    for var in vars_not_to_impute:
        if var in num_na_train:
            detected_vars.append(var)
            num_na_train.remove(var)

    # Do imputation
    if len(num_na_train) > 0:
        if method == "knn":
            imputer = KNNImputer(n_neighbors=n_neighbours)
            X_train_in[num_na_train] = imputer.fit_transform(X_train_in[num_na_train])
            X_test_in[num_na_train] = imputer.transform(X_test_in[num_na_train])

        elif method == "mean":
            for var in num_na_train:
                test_mean = X_train_in[var].mean()
                X_train_in[var] = X_train_in[var].fillna(test_mean)
                X_test_in[var] = X_test_in[var].fillna(test_mean)

        elif method == "median":
            for var in num_na_train:
                test_median = X_train_in[var].median()
                X_train_in[var] = X_train_in[var].fillna(test_median)
                X_test_in[var] = X_test_in[var].fillna(test_median)

        elif method == "minus_9999":
            X_train_in[num_na_train] = X_train_in[num_na_train].fillna(-9999)
            X_test_in[num_na_train] = X_test_in[num_na_train].fillna(-9999)

    # Attach target to X_train_in
    Xy_train_out = X_train_in.copy()
    Xy_train_out[target_in] = Xy_train_in[target_in]

    Xy_test_out = X_test_in.copy()
    Xy_test_out[target_in] = Xy_test_in[target_in]

    if verbose:
        print(
            f"impute_numerical_na():",
            f"\n - Shape of Xy_train before imputation: {Xy_train_in.shape}",
            f"\n - Shape of Xy_train before imputation: {Xy_train_out.shape}",
            f"\n",
            f"\n - Shape of Xy_test before imputation: {Xy_test_in.shape}",
            f"\n - Shape of Xy_test before imputation: {Xy_test_out.shape}",
            f"\n",
            f"\n - Out of {len(all_numerics)}, {len(num_na_train)} had NA values and were imputed using KNN with {n_neighbours} neighbours.",
            # f"\n - Imputed variables: {' | '.join(sorted(num_na_train))}",
        )

    if len(detected_vars) > 0:
        print(
            f"\n - ❌❌❌ Variables {detected_vars} had NA values but are not meant to be imputed! They were not imputed but their values should be fixed! ❌❌❌"
        )

    return Xy_train_out, Xy_test_out, num_na_train
# -----------------------------------------------------------------------------------------------
def overlay_barplot(ax, df1, df1_name, df2, df2_name, variable, subtitle=None):
    # Number of levels in variable
    nvar = len(set([var for var in df1[variable]] + [var for var in df2[variable]]))

    # Calculate the relative frequencies for each level in df1
    df1_counts = df1[variable].value_counts(normalize=True).sort_values(ascending=False)
    df1_counts = df1_counts * 100

    # Calculate the relative frequencies for each level in df2
    df2_counts = df2[variable].value_counts(normalize=True).sort_values(ascending=False)
    df2_counts = df2_counts * 100

    # Combine the counts into a single dataframe
    df_counts = pd.concat([df1_counts, df2_counts], axis=1)
    df_counts.columns = [df1_name, df2_name]

    # Plotting the bar plot
    ax.barh(
        df_counts.index, df_counts[df1_name], align="center", label=df1_name, alpha=0.5
    )
    ax.barh(
        df_counts.index, df_counts[df2_name], align="center", label=df2_name, alpha=0.5
    )

    # Adding titles and labels
    ax.set_title("Overlaying Bar Plot of variable of total " + str(nvar) + " strata")
    ax.set_xlabel("Relative Frequency [%]")
    ax.set_ylabel("Level of variable")
    ax.legend()

    if subtitle is not None:
        ax.text(
            0.5,
            0.95,
            subtitle,
            transform=ax.transAxes,
            ha="center",
            va="center",
        )
# -----------------------------------------------------------------------------------------------
def get_weights_from_y(y_in, method="none"):
    if method == "none":
        return np.arange(1, len(y_in)+1)
    if method == "squared":
        return y_in**2
    if method == "cubic":
        return y_in**3
    if method == "quadratic":
        return y_in**4
    if method == "inverse":
        return 1 / y_in
    if method == "inverse_squared":
        return 1 / y_in**2
# -----------------------------------------------------------------------------------------------
def aggregate_strata_with_too_little_obs(
    Xy_in, strata_vars, do_fold_too, split_test, cv_fold, seed_nr,
):
    # Start printing call
    print(
        f"wrangle_stratification_of_dataset():",
        end="",
    )

    # ----------------------------------------
    # Get df_all_strata for splitting
    df_all_strata = merge_small_strata_to_one(Xy_in, strata_vars, "split", cv_fold)

    # Pick arbitrary target that is not in strata_vars
    random_column = random.choice(
        df_all_strata.drop(columns=strata_vars + ["test_train_strata"]).columns
    )
    # Split dataset into train and test set
    xtr, xte, ytr, yte = train_test_split(
        df_all_strata.drop(random_column, axis=1),
        df_all_strata[random_column],
        test_size=split_test,
        random_state=seed_nr,
        stratify=df_all_strata["test_train_strata"],
    )

    # strata_groups = df_all_strata["test_train_strata"].unique()

    if not do_fold_too:
        fig, ax = plt.subplots(1, figsize=(12.5, 25))
        display()
        overlay_barplot(ax, xtr, "Train", xte, "Test", "test_train_strata", "All Data")

        return df_all_strata.reset_index(drop=True)
    else:
        pass

    # Do folding too
    # ----------------------------------------
    print(
        f"\n - Check strata aggregation needed for splitted X_train to successfully create {cv_fold} CV-folds...",
        end="",
    )

    # Take training split from above
    new_df = pd.concat([xtr.reset_index(drop=True), ytr.reset_index(drop=True)], axis=1)
    new_df = attach_test_train_strata(
        new_df, strata_vars
    )  # Attach test_train_strata variable
    new_df = merge_small_strata_to_one(
        new_df, strata_vars, "fold", cv_fold
    )  # Merge strata for folding

    # Define folding
    skf = StratifiedKFold(n_splits=cv_fold, shuffle=True)

    # Run folding to see if it works with the merged strata
    for fold, (train_index, test_index) in enumerate(
        skf.split(new_df, new_df["test_train_strata"])
    ):
        xtr, xte = (
            new_df.iloc[train_index],
            new_df.iloc[test_index],
        )

    # Get strata groups that works for split and fold
    final_strata_groups = new_df["test_train_strata"].unique()

    # FINAL MERGING OF STRATA AND DISPLAY
    # ----------------------------------------
    print(
        f"\n - ✅ Got the final groups for strata aggregation.",
        f"\n  - Running routine again to create final distribution plots of train/test splits and train/validation folds.",
        end="",
    )

    df_out = Xy_in.copy()  # Get copy of input data
    df_out = attach_test_train_strata(
        df_out, strata_vars
    )  # Attach test_train_strata variable
    org_number_of_strata = df_out[
        "test_train_strata"
    ].nunique()  # Check how many stratification there were originally

    # If strata in df_out are not in final_strata_groups, then replace with "others"
    for s in df_out["test_train_strata"].unique():
        if s not in final_strata_groups:
            df_out["test_train_strata"] = df_out["test_train_strata"].replace(
                s, "others"
            )

    # Print information
    print(
        f"\n  - For successful splitting and {cv_fold} CV-folds, {org_number_of_strata} strata were merged into {len(final_strata_groups)} strata.",
        f"\n  - Stratas with too little observations were merged into 'others', which makes up {round(df_out[df_out['test_train_strata'] == 'others'].shape[0]/df_out.shape[0]*100, 2)}% of the data.",
    )

    # Make figures (for this, we first need to do the splitting and folding again!)
    # ----------------------------------------
    # SPLIT
    # Pick arbitrary target that is not in test_train_strata
    random_column = random.choice(
        df_out.drop(columns=strata_vars + ["test_train_strata"]).columns
    )
    # Split dataset into train and test set
    xtr, xte, ytr, yte = train_test_split(
        df_out.drop(random_column, axis=1),
        df_out[random_column],
        test_size=split_test,
        random_state=seed_nr,
        stratify=df_all_strata["test_train_strata"],
    )
    # Make plot
    fig, ax = plt.subplots(1, figsize=(25, 25))
    display()
    overlay_barplot(ax, xtr, "Train", xte, "Test", "test_train_strata", "All Data")

    # FOLD
    # Take training split from above
    new_df = pd.concat([xtr.reset_index(drop=True), ytr.reset_index(drop=True)], axis=1)
    # No need to re-attach test_train_strata variable as above, because it is already attached
    # from the definition of df_out.

    # Define folding (take same as above, so no need to redefine)
    # Initiate plot
    fig, axs = plt.subplots(1, cv_fold, figsize=(25, 25))
    # Do folds
    for fold, (train_index, test_index) in enumerate(
        skf.split(new_df, new_df["test_train_strata"])
    ):
        xtr, xte = (
            new_df.iloc[train_index],
            new_df.iloc[test_index],
        )

        # Make plot
        axs[fold] = overlay_barplot(
            axs[fold],
            xtr,
            "Train",
            xte,
            "Test",
            "test_train_strata",
            str("Fold " + str(fold + 1)),
        )

    # Return final dataframe with correctly merged strata
    return df_out.reset_index(drop=True)
# -----------------------------------------------------------------------------------------------

# ----------------------------------------
# DEFINE FUNCTIONS FOR STRATIFICATION DISTRIBUTION CHECKS
# ----------------------------------------
# Attach test_train_strata dummy
def attach_test_train_strata(df_in, var_list):
    df_in["test_train_strata"] = ""

    # Attach test_train_strata variables
    for var in var_list:
        df_in["test_train_strata"] = df_in["test_train_strata"] + (
            "_" + df_in[var].astype(str)
        )
    return df_in

# Define merger function
def merge_small_strata_to_one(Xy_in, strata_vars, split_or_fold, cv_fold):
    # Start printing
    print(
        f"\n - Merging strata for successful {split_or_fold}...",
        end="",
    )

    # Get min. number of observations per group
    if split_or_fold == "split":
        min_obs_per_group = 2
        print(
            f"\n  - For splitting, the min. number of observations per group is {min_obs_per_group}.",
            end="",
        )
    if split_or_fold == "fold":
        min_obs_per_group = cv_fold
        print(
            f"\n  - For cv-folds, the min. number of observations per group is {cv_fold}.",
            end="",
        )

    # Aggregate strata according to min_obs_per_group
    # ----------------------------------------
    df_all_strata = Xy_in.copy()  # Get temporary df
    df_all_strata = attach_test_train_strata(
        df_all_strata, strata_vars
    )  # Attach test_train_strata variable

    stratification_lvls = df_all_strata[
        "test_train_strata"
    ].nunique()  # Check how many stratification lvls there are

    print(
        f"\n  - Stratification levels: {stratification_lvls}, based on variables: {strata_vars}.",
        end="",
    )

    # Get df with observations per strata
    df_observations_per_strata = df_all_strata.groupby("test_train_strata").size()
    # Reduce df to only hold strata with less than min_obs_per_group observations
    df_strata_with_too_little_obs = df_observations_per_strata[
        df_observations_per_strata < min_obs_per_group
    ]

    # Get sum of observations within strata with too little obs
    sum_of_observations_within_strata_with_too_little_obs = (
        df_strata_with_too_little_obs.sum()
    )

    # Print results
    org_numb_strata = df_all_strata["test_train_strata"].nunique()

    print(
        f"\n  - Out of {df_all_strata.shape[0]} observations, there are: {sum_of_observations_within_strata_with_too_little_obs} ({round(sum_of_observations_within_strata_with_too_little_obs/df_all_strata.shape[0]*100, 2)}%) that are in strata with less than {min_obs_per_group} observations.",
        f"\n  - These {sum_of_observations_within_strata_with_too_little_obs} observations from {df_strata_with_too_little_obs.shape[0]} stratas will be put into strata 'others'",
        end="",
    )

    # Replace strata with too little obs with "others"
    for s in df_strata_with_too_little_obs.index:
        df_all_strata["test_train_strata"] = df_all_strata[
            "test_train_strata"
        ].replace(s, "others")

    fin_numb_strata = df_all_strata["test_train_strata"].nunique()

    print(
        f"\n  - From {org_numb_strata} strata, {fin_numb_strata} strata remain after merging strata with less than {min_obs_per_group} observations.",
    )

    return df_all_strata
# -----------------------------------------------------------------------------------------------
def split_into_quantiles(arr, n_groups):
    labels = [f"{i/n_groups*100:.0f}-{(i+1)/n_groups*100:.0f}%" for i in range(n_groups)]
    groups = pd.qcut(arr, n_groups, labels=labels)
    print(f"Created {n_groups} groups: {labels}")
    return groups
# -----------------------------------------------------------------------------------------------
def set_best_rf_params(make_classification="regression"):
    if not make_classification:
        # Return best variables from previous search
        best_params = {
            "n_estimators": 500,
            # "min_samples_split": 2,
            # "min_samples_leaf": 1,
            "max_features": 0.1,  # "sqrt",  # 0.25,
            "max_depth": 25,
            "bootstrap": True,
            "criterion": "squared_error",  # ["squared_error", "absolute_error", "friedman_mse", "poisson"]
        }

    if make_classification:
        best_params = {
            "n_estimators": 500,
            # "min_samples_split": 2,
            # "min_samples_leaf": 1,
            "max_features": 0.1,  # "sqrt",  # 0.25,
            "max_depth": 25,
            "bootstrap": True,
            "criterion": "gini",  # ["squared_error", "absolute_error", "friedman_mse", "poisson"]
        }

    return best_params
# -----------------------------------------------------------------------------------------------
def get_tune_grid_regression():
    param_grid = {
        "n_estimators": [
            # 10,
            # 100,
            250,
            # 500,
            # 1000
        ],  # [int(x) for x in np.linspace(start=10, stop=2500, num=5)],
        "max_features": [
            # "sqrt",
            # "log2",
            0.01,
            0.1,
            0.25,
            # 0.5,
            # 0.75,
            # 1,
        ],
        # + [x for x in np.linspace(start=0.1, stop=0.8, num=5)],
        "max_depth": [
            1,
            # 5,
            8,
            15,
            # 20,
            25, 
            # 100,
            ],  # [int(x) for x in np.linspace(1, 50, num=5)],
        "criterion": [
            "squared_error"
        ],  # ["squared_error", "absolute_error", "friedman_mse", "poisson"],
        # "min_samples_split": [2, 5, 10
        # "min_samples_leaf": [1, 2, 4]
        # "bootstrap": [True, False],
    }
    return param_grid
    
# -----------------------------------------------------------------------------------------------
def get_tune_grid_classification():
    param_grid = {
        'n_estimators': [50, 100, 200],
        'max_depth': [1, 5, 15, 30],
        # 'min_samples_split': [2, 5, 10],
        # 'min_samples_leaf': [1, 2, 4],
        'max_features': [0.01, 0.1, 0.2, 'sqrt'],
        'bootstrap': [True],
        'criterion': ['gini', 'entropy']
    }
    
    return param_grid

# -----------------------------------------------------------------------------------------------
def plot_quantiles(array, num_quantiles, variable_name, directory=None):
    # Calculate quantiles
    quantiles = np.percentile(array, np.linspace(0, 100, num_quantiles + 1))

    # Create histogram
    sns.histplot(array, x=target)
    # plt.hist(array, bins="auto", color="royalblue", alpha=0.7, rwidth=0.85)

    # Add vertical lines for quantiles
    for quantile in quantiles:
        plt.axvline(quantile, color="red", linestyle="--")

    # Set labels and title
    plt.xlabel("Value")
    plt.ylabel("Frequency")
    plt.title(f"Histogram of {variable_name} with {num_quantiles} Quantiles")

    # Save if input dir is given
    if directory is not None:
        plt.savefig(f"{directory}/fig_histogram_{variable_name}.png")

    # Show the plot
    plt.show()
# -----------------------------------------------------------------------------------------------
def pdp_mp_wrapper(target, **kwargs):
    return PartialDependenceDisplay.from_estimator(target=target, **kwargs)
# -----------------------------------------------------------------------------------------------