compute_clouts <- function(elasticities) {
  diag(elasticities) <- 0
  clouts <- colSums(elasticities)
  return(clouts)
}

compute_vulns <- function(elasticities) {
  diag(elasticities) <- 0
  vulns <- rowSums(elasticities)
  return(vulns)
}  


heavy_clouts <- compute_clouts(heavy_elasticities_clean)
heavy_vulns <- compute_vulns(heavy_elasticities_clean)

light_clouts <- compute_clouts(light_elasticities_clean)
light_vulns <- compute_vulns(light_elasticities_clean)

heavy_stats <- data_frame(cust = rep("heavy", length(heavy_clouts)),
                          brand = names(heavy_clouts),
                          clout = heavy_clouts,
                          vuln = heavy_vulns)

light_stats <- data_frame(cust = rep("light", length(light_clouts)),
                          brand = names(light_clouts),
                          clout = light_clouts,
                          vuln = light_vulns)

stats <- bind_rows(heavy_stats, light_stats)

rm(list = c("heavy_clouts", "heavy_vulns", "light_clouts", "light_vulns"))


stats %>% 
  ggplot(aes(x = vuln, y = clout)) +
  geom_point(aes(colour = brand), size = 5) +
  facet_grid(. ~ cust) 



