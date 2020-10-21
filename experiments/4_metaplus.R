# metaplus experiments
# ==============================================================================
rm(list = ls())
library(metaplus)
library(dplyr)
source("settings.R")
source("functions.R")

# load data --------------------------------------------------------------------
dfs <- load_data(FOLDERS$data_folder, PARAMS$num_trials)

# define function --------------------------------------------------------------
fit_data <- function(data, inlier_percentage = 0.8) {
  # fit model
  fit <- metaplus(
    yi = obs,
    sei = obs_sd,
    mods = x1,
    slab = study_id,
    justfit = TRUE,
    random = "t-dist",
    data = data
  )
  
  # extract information
  num_obs <- dim(data)[1]
  num_outliers <- round((1 - inlier_percentage)*num_obs)
  
  # extract results
  coef <- fit$fittedmodel@coef
  beta0 <- coef[['muhat']]
  beta1 <- coef[['x1']]
  gamma <- coef[['tau2']]
  
  # extract outliers
  outliers <- pick_outliers(data, beta0, beta1, num_outliers)
  num_outliers_detected <- sum(data$outliers & outliers)
  
  data.frame(
    model = "metaplus",
    beta0 = beta0,
    beta1 = beta1,
    gamma = gamma,
    true_outlier_detected = num_outliers_detected,
    sample_size = num_obs
  )
}

# fit all the data -------------------------------------------------------------
df_results <- get_results(dfs, fit_data)

# save results -----------------------------------------------------------------
results_file_path <- paste(FOLDERS$results_folder, "case_meta.csv", sep="/")
save_results(df_results, results_file_path)
