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



# Step 3 - run visualisation scripts in order ----------------------------------
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

# Custom visualisations (create plot objects)
source("./r/files/801_clout_and_vuln_map.R")
source("./r/files/802_variable_importance_plot.R")
source("./r/files/803_buying_behaviour_charts.R")


# Step 3 - write plots to file -------------------------------------------------
if (!dir.exists("./visualisations")) {
  dir.create("./visualisations")
}
# Save clout and vulnerability map
ggsave("./visualisations/clout_and_vuln_map.svg", clout_and_vuln_map)

# Save variable importance plot
svg(filename = "./visualisations/var_imp_plot.svg")
var_imp(rf)
dev.off()

# Find all buying behaviour plots in the workspace (all prefaced with "bb_")
bbplots <- ls()[grep("bb_", ls())] 

# Loop over buy-behav plots and write to SVG files
res <- lapply(bbplots, function(x) ggsave(paste0("./visualisations/", x,".svg"),
                                   eval(parse(text = x))))
rm(res)

