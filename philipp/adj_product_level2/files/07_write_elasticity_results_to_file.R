# Step 0 - prepare environment -------------------------------------------------
if (!dir.exists("./philipp/adj_product_level2/results")) {
  dir.create("./philipp/adj_product_level2/results")
}

# Step 1 - write to file -------------------------------------------------------
# Heavy elasticity matrix
as.data.frame(heavy_elasticities_clean) %>% 
  write_csv("./philipp/adj_product_level2/results/heavy_elasticities.csv")

# Light elasticity matrix
as.data.frame(light_elasticities_clean) %>% 
write_csv("./philipp/adj_product_level2/results/light_elasticities.csv")

# Clout and vulnerability stats
write_csv(clout_and_vuln_stats, "./philipp/adj_product_level2/results/clout_and_vuln_stats.csv")
