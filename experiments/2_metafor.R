# metafor experiments
# ==============================================================================
rm(ls = list())
library(metafor)
library(readr)

num_trials <- 30
data_folder <- "./data"

# load data
# ------------------------------------------------------------------------------
dfs <- list()
for (i in seq_len(num_trials)) {
  dfs[[i]] <- read_csv(paste0(data_folder, "/data_", i, ".csv"))
}
