# Utility functions
# ==============================================================================
library(readr)

create_dir <- function(path) {
  if (!file.exists(path)) dir.create(path)
}

sim_data <- function(params, seed = NULL, add_outliers = FALSE) {
  if (is.null(seed)) set.seed(seed)
  # create cov
  num_obs <- params$num_studies * params$obs_per_study
  x1 <- runif(n = num_obs, min = 0, max = 10)
  
  # create measurements
  obs <- params$beta0 + params$beta1 * x1
  
  # create measurement errors
  obs_err <- rnorm(n = num_obs, sd = params$obs_sd)
  obs_sd <- rep(params$obs_sd, num_obs)
  
  # create random effects
  re <- rnorm(n = params$num_studies, sd = sqrt(params$gamma))
  re <- rep(re, each = params$obs_per_study)

  # create study_id
  study_id <- rep(1:params$num_studies, each = params$obs_per_study)
  
  # add outliers (optional)
  outliers <- rep(0, num_obs)
  if (add_outliers) {
    num_eff_x1 <- sum(x1 >= 6)
    num_outliers <- round(num_obs*params$pct_outliers)
    indices <- order(x1)[sample((num_obs-num_eff_x1):num_obs, size = num_outliers)]
    outliers[indices] <- params$outlier_offset + sign(params$outlier_offset) * abs(rnorm(n = num_outliers, sd = params$outlier_sd))
  }
  
  # combine observations
  obs <- obs + obs_err + re + outliers
  
  data = data.frame(
    x1 = x1,
    obs = obs,
    obs_sd = obs_sd,
    re = re,
    study_id = study_id,
    outliers = outliers
  )
  return(data)
}

load_data <- function(data_folder, num_trials) {
  dfs <- list()
  for (i in seq_len(num_trials)) {
    dfs[[i]] <- read_csv(paste0(data_folder, "/data_", i, ".csv"))
  }
  dfs
}

pick_outliers <- function(data, beta0, beta1, num_outliers, scale_resi = T) {
  pred <- beta0 + data$x1*beta1
  resi <- data$obs - pred
  if (scale_resi) {
    resi <- resi/data$obs_sd
  }
  outliers <- rep(0, dim(data)[1])
  sort_result <- sort(resi, decreasing = TRUE, index.return = TRUE)
  outliers[sort_result$ix][1:num_outliers] <- 1
  outliers
}

get_results <- function(dfs, fit_data) {
  results <- list()
  for (i in seq_along(dfs)) {
    results[[i]] <- fit_data(dfs[[1]])
  }
  df_results <- do.call(rbind, results)
  df_results$data_id <- seq_along(dfs)
  df_results
}

save_results <- function(df_results, results_file_path) {
  if (file.exists(results_file_path)) {
    prev_df_results <- read_csv(results_file_path)
    df_results <- rbind(prev_df_results, df_results)
  }
  write_csv(df_results, results_file_path)
}
