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
source("./r/files/804_brands_purchased_plot.R")


# Step 3 - write plots to file -------------------------------------------------
if (!dir.exists("./visualisations")) {
  dir.create("./visualisations")
}
# Save clout and vulnerability map
ggsave("./visualisations/clout_and_vuln_map.svg", clout_and_vuln_map, height = 7, width = 8)

# Save variable importance plot
svg(filename = "./visualisations/var_imp_plot.svg")
var_imp(rf)
dev.off()

# Find all buying behaviour plots in the workspace (all prefaced with "bb_")
bbplots <- ls()[grep("bb_", ls())] 

# Loop over buy-behav plots and write to SVG files
res <- lapply(bbplots, function(x) ggsave(filename = paste0("./visualisations/", x,".svg"),
                                          plot = eval(parse(text = x)), height = 6, width = 11))
rm(res)

# Save prop brands purchased plot
ggsave("./visualisations/brands_purchased.svg", brands_purchase_plot, height = 7, width = 9)