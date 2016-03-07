# Step 0 - load packages -------------------------------------------------------
library(dplyr)

# Step 1 - source user-defined functions----------------------------------------
source("./r/functions/normalise.R")


# Step 2 - run scripts in order ------------------------------------------------
source("./r/files/00_clean_and_filter.R")
source("./r/files/01_light_vs_heavy.R")
source("./r/files/02_clean_brands.R")
