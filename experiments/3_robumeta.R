# robumeta experiments
# ==============================================================================
rm(ls = list())
library(robumeta)
library(readr)
library(dplyr)

num_trials <- 30
data_folder <- "./data"
result_folder <- "./results"

# load data
# ------------------------------------------------------------------------------
dfs <- list()
for (i in seq_len(num_trials)) {
  dfs[[i]] <- read_csv(paste0(data_folder, "/data_", i, ".csv"))
}

# define function
# ------------------------------------------------------------------------------
data = dfs[[1]]
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
  
  # extra outliers, problem: pred might not have random effects
  pred <- beta0 + beta1*data$x1
  scaled_residual <- abs(data$obs - pred)/data$obs_sd
  outliers <- rep(0, dim(data)[1])
  sort_result <- sort(scaled_residual, decreasing = TRUE, index.return = TRUE)
  outliers[sort_result$ix][1:num_outliers] <- 1
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

# fit all the data
# ------------------------------------------------------------------------------
results <- list()
for (i in seq_len(num_trials)) {
  results[[i]] <- fit_data(dfs[[1]])
}
df_results <- do.call(rbind, results)
df_results$data_id <- 1:num_trials

# save results
# ------------------------------------------------------------------------------
results_file_path = paste(result_folder, "case_meta.csv", sep="/")
if (file.exists(results_file_path)) {
  prev_df_results <- read_csv(results_file_path)
  df_results <- rbind(prev_df_results, df_results)
}
write_csv(df_results, results_file_path)
