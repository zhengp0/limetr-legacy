# Create data set for case 1
# ==============================================================================
rm(list = ls())
library(readr)
source("functions.R")
source("settings.R")

# create directories -----------------------------------------------------------
for (i in seq_along(folders)) {
  create_dir(FOLDERS[[i]])
}

# create data ------------------------------------------------------------------
set.seed(SEED)
for (i in 1:params$num_trials) {
  data <- sim_data(PARAMS, add_outliers = ADD_OUTLIERS)
  write_csv(data, paste0(FOLDERS$data_folder, "/data_", i, ".csv"))
}
