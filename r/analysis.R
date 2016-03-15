# Step 0 - load packages -------------------------------------------------------
library(dplyr)
library(tidyr)
library(broom)
library(readr)
library(randomForest)
library(ggplot2)

# Step 1 - source user-defined functions----------------------------------------
source("./r/functions/normalise.R")
source("./r/functions/toproper.R")

# Set up custom theme to be applied to all plot objects
theme <- theme(legend.position = "bottom",
               axis.text.y = element_text(size = 16, colour = "black"),
               axis.text.x = element_text(size = 16, colour = "black"),
               legend.text = element_text(size = 16),
               legend.title = element_text(size = 16),
               title = element_text(size = 16),
               strip.text = element_text(size = 16, colour = "black"),
               strip.background = element_rect(fill = "white"),
               panel.grid.minor.x = element_blank(),
               panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
               panel.grid.minor.y = element_line(colour = "lightgrey", linetype = "dotted"),
               panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
               panel.margin.y = unit(0.1, units = "in"),
               panel.background = element_rect(fill = "white", colour = "lightgrey"),
               panel.border = element_rect(colour = "black", fill = NA))


# Step 2 - run scripts in order ------------------------------------------------
# Run Analysis code
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

# Run plotting code
source("./r/files/10_clout_and_vuln_map.R")

