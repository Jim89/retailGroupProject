# Step 0 - prepare environment -------------------------------------------------
# Define function to calculate brand clout(s) from elasticity matrix (sum of 
# column values, ignoring diagonal)
compute_clouts <- function(elasticities) {
  diag(elasticities) <- 0
  elasticities[elasticities < 0] <- 0
  clouts <- colSums(elasticities)
  return(clouts)
}

# Define function to calculate brand vulnerability(s) from elasticity matrix (sum of 
# row values, ignoring diagonal)
compute_vulns <- function(elasticities) {
  diag(elasticities) <- 0
  elasticities[elasticities < 0] <- 0
  vulns <- rowSums(elasticities)
  return(vulns)
}  

# Step 1 - Compute stats for heavy users ---------------------------------------
heavy_clouts <- compute_clouts(heavy_elasticities_clean)
heavy_vulns <- compute_vulns(heavy_elasticities_clean)

# Step 2 - Compute stats for light users ---------------------------------------
light_clouts <- compute_clouts(light_elasticities_clean)
light_vulns <- compute_vulns(light_elasticities_clean)

# Step 3 - Create data frame for light and heavy users -------------------------
# Heavy users
heavy_stats <- data_frame(cust = rep("heavy", length(heavy_clouts)),
                          brand = names(heavy_clouts),
                          clout = heavy_clouts,
                          vuln = heavy_vulns)

# Light users
light_stats <- data_frame(cust = rep("light", length(light_clouts)),
                          brand = names(light_clouts),
                          clout = light_clouts,
                          vuln = light_vulns)

# Step 4 - Create single data frame with stats for both sets of users ----------
clout_and_vuln_stats <- bind_rows(heavy_stats, light_stats)

# Step 5 - Clean up ------------------------------------------------------------
rm(list = c("heavy_clouts", "heavy_vulns", "light_clouts", "light_vulns",
            "heavy_stats", "light_stats"))
gc()


