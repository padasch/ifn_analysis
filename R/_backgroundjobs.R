# Make wide location dataset (CHUNK TAKES ABOUT 6 MINUTES TO RUN!)
# Start Timer
tic()
# Get dataframes stating which variable is sampled how often and when
loc_vars  <- get_measurement_frequency_of_vars(df_loc,  "location")
tree_vars <- get_measurement_frequency_of_vars(df_tree, "tree")

# Widen dataframes
df_loc_wide  <- widen_dataframe(df_loc,  loc_vars,  "location")
df_tree_wide <- widen_dataframe(df_tree, tree_vars, "tree")

# input <- df_loc;  df_vars <- loc_vars;  data_type <- "location"
# input <- df_tree; df_vars <- tree_vars; data_type  <- "tree"

# Quality check for duplicates:
if (length(unique(df_loc_wide$idp)) != nrow(df_loc_wide)) {stop("QC FAILED!")}
if (length(unique(df_tree_wide$tree_id)) != nrow(df_tree_wide)) {stop("QC FAILED!")}

# Combine and clean dataframes to one large wide one with one tree per row
vars_clean   <- c("idp", "visite_1", "visite_2", "campagne_1", "campagne_2")
df_loc_wide  <- df_loc_wide  |> mutate(across(all_of(vars_clean), factor))
df_tree_wide <- df_tree_wide |> mutate(across(all_of(vars_clean), factor))

df_comb <- 
  left_join(
    df_tree_wide, 
    df_loc_wide, 
    by = vars_clean)

# End Timer
toc()
beep(2)

# Save data
load_or_save_latest_file("df_comb", "save")
