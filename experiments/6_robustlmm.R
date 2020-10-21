# robustlmm experiments
# ==============================================================================
rm(list = ls())
library(robustlmm)
source("settings.R")
source("functions.R")

# load data
# ------------------------------------------------------------------------------
dfs <- load_data(FOLDERS$data_folder, PARAMS$num_trials)

# define function
# ------------------------------------------------------------------------------
fit_data <- function(data, inlier_percentage = 0.8) {
  # fit model
  fit <- rlmer(obs ~ x1 + (1 | study_id), data = data)
  
  # extract information
  num_obs <- dim(data)[1]
  num_outliers <- round((1 - inlier_percentage)*num_obs)
  
  # extract results
  result_summary <- summary(fit)
  coefs <- as.numeric(getInfo(fit)$coef)
  beta0 <- coefs[[1]]
  beta1 <- coefs[[2]]
  gamma <- attr(result_summary$varcor$study_id, "stddev")[[1]]^2
  delta <- result_summary$sigma^2
  
  # extract outliers
  outliers <- pick_outliers(data, beta0, beta1, num_outliers,
                            scale_resi = FALSE)
  num_outliers_detected <- sum(data$outliers & outliers)
  
  data.frame(
    model = "robustlmm",
    beta0 = beta0,
    beta1 = beta1,
    gamma = gamma,
    delta = delta,
    true_outlier_detected = num_outliers_detected,
    sample_size = num_obs
  )
}

# fit all the data -------------------------------------------------------------
df_results <- get_results(dfs, fit_data)

# save results -----------------------------------------------------------------
results_file_path <- paste(FOLDERS$results_folder, "case_long.csv", sep="/")
save_results(df_results, results_file_path)
