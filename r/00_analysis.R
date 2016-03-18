# Step 0 - load packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(randomForest)
library(ggplot2)

# Clean up
rm(list = ls())

# Step 1 - source user-defined functions----------------------------------------
source("./r/functions/normalise.R")
source("./r/functions/toproper.R")


# Step 2 - run build cripts in order -------------------------------------------
# Data clean and preparation
source("./r/files/000_clean_and_filter.R")
source("./r/files/001_light_vs_heavy.R")
source("./r/files/002_clean_brands.R")
source("./r/files/003_prepare_for_modelling.R")

# Elasticity modelling
source("./r/files/101_heavy_elast.R")
source("./r/files/102_light_elast.R")
source("./r/files/103_clout_and_vuln_stats.R")
source("./r/files/104_write_elasticity_results_to_file.R")

# Buying behaviour modelling
source("./r/files/201_buying_behaviour_data.R")
source("./r/files/202_buying_behaviour_classify.R")

# Co-occurence and switching matrices
source("./r/files/401_cooccurence_matrices.R")
source("./r/files/402_switching_matrices.R")



