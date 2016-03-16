# Step 0 - prepare environment -------------------------------------------------
if (!dir.exists("./philipp/adj_no_prop/results")) {
  dir.create("./philipp/adj_no_prop/results")
}

# Step 1 - write to file -------------------------------------------------------
# Heavy elasticity matrix
as.data.frame(heavy_elasticities_clean) %>% 
  write_csv("./philipp/adj_no_prop/results/heavy_elasticities.csv")

# Light elasticity matrix
as.data.frame(light_elasticities_clean) %>% 
write_csv("./philipp/adj_no_prop/results/light_elasticities.csv")

# Clout and vulnerability stats
write_csv(clout_and_vuln_stats, "./philipp/adj_no_prop/results/clout_and_vuln_stats.csv")
