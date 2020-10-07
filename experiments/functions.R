# Utility functions
# ==============================================================================

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
  
  # create random effects
  re <- rnorm(n = params$num_studies, sd = sqrt(params$gamma))
  re_full <- rep(re, each = params$obs_per_study)
  
  # add outliers (optional)
  outliers <- rep(0, num_obs)
  if (add_outliers) {
    num_eff_x1 <- sum(x1 >= 6)
    num_outliers <- round(num_obs*params$pct_outliers)
    indices <- order(x1)[sample((num_obs-num_eff_x1):num_obs, size = num_outliers)]
    outliers[indices] <- params$outlier_offset + sign(params$outlier_offset) * abs(rnorm(n = num_outliers, sd = params$outlier_sd))
  }
  
  # combine observations
  obs <- obs + obs_err + re_full + outliers
  
  data = data.frame(
    x1 = x1,
    obs = obs,
    re = re_full,
    outliers = outliers
  )
  return(data)
}