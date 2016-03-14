# Step 0 - prepare environment -------------------------------------------------
if (!dir.exists("./data/results")) {
  dir.create("./data/results")
}

# Step 1 - write to file -------------------------------------------------------
# Heavy elasticity matrix
as.data.frame(heavy_elasticities_clean) %>% 
  write_csv("./data/results/heavy_elasticities.csv")

# Light elasticity matrix
as.data.frame(light_elasticities_clean) %>% 
write_csv("./data/results/light_elasticities.csv")

# Clout and vulnerability stats
write_csv(clout_and_vuln_stats, "./data/results/clout_and_vuln_stats.csv")
