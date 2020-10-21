# robumeta experiments
# ==============================================================================
rm(list = ls())
library(robumeta)
library(readr)
library(dplyr)
source("settings.R")
source("functions.R")

# load data --------------------------------------------------------------------
dfs <- load_data(FOLDERS$data_folder, PARAMS$num_trials)

# define function
# ------------------------------------------------------------------------------
fit_data <- function(data, inlier_percentage = 0.8) {
  # add variance
  data <- mutate(data, obs_var = obs_sd^2)
  # fit model
  fit <- robu(obs ~ x1,
              studynum = study_id,
              var.eff.size = obs_var,
              data = data)
  
  # extract information
  num_obs <- dim(data)[1]
  num_outliers <- round((1 - inlier_percentage)*num_obs)
  
  # extract results
  beta0 <- fit$reg_table$b.r[1]
  beta1 <- fit$reg_table$b.r[2]
  gamma <- fit$mod_info$tau.sq
  
  # extract outliers
  outliers <- pick_outliers(data, beta0, beta1, num_outliers)
  num_outliers_detected <- sum(data$outliers & outliers)
  
  data.frame(
    model = "robumeta",
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
