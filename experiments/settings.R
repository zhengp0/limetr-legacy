# global settings of the experiments
# ==============================================================================

# directories ------------------------------------------------------------------
FOLDERS = list(
  data_folder = "./data",
  results_folder = "./results",
  pred_folder = "./pred",
  plots_folder = "./plots"
)

# simulation parameters --------------------------------------------------------
PARAMS <- list(
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
SEED <- 123
ADD_OUTLIERS <- TRUE

