# Step 0 - load packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(randomForest)
library(ggplot2)

# Step 1 - source user-defined functions----------------------------------------
source("./r/functions/normalise.R")


# Step 2 - run scripts in order ------------------------------------------------
source("./r/files/00_clean_and_filter.R")
source("./r/files/01_light_vs_heavy.R")
source("./r/files/02_clean_brands.R")
source("./r/files/03_prepare_for_modelling.R")
source("./r/files/04_heavy_elast.R")
source("./r/files/05_light_elast.R")
source("./r/files/06_clout_and_vuln_stats.R")
source("./r/files/07_write_elasticity_results_to_file.R")
source("./r/files/08_buying_behaviour_data.R")
source("./r/files/09_buying_behaviour_classify.R")

