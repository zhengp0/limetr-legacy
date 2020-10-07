# Create data set for case 1
# ==============================================================================
rm(list = ls())
library(readr)
source("functions.R")

# settings
# ------------------------------------------------------------------------------
params <- list(
  num_trials = 30,
  beta0 = 0,
  beta1 = 5,
  gamma = 36,
  num_studies = 10,
  obs_per_study = 10,
  obs_sd = 4,
  outlier_offset = -30,
  outlier_sd = 80,
  pct_outliers = 0.15,
  pct_outliers_assumed = 0.2
)
seed <- 123
add_outliers <- TRUE


# create directories
# ------------------------------------------------------------------------------
folders = list(
  data_folder = "./data",
  results_folder = "./results",
  pred_folder = "./pred",
  plots_folder = "./plots"
)

for (i in seq_along(folders)) {
  create_dir(folders[[i]])
}

# create data
# ------------------------------------------------------------------------------
set.seed(seed)
for (i in 1:params$num_trials) {
  data <- sim_data(params, add_outliers = add_outliers)
  write_csv(data, paste0(folders$data_folder, "/data_", i, ".csv"))
}

