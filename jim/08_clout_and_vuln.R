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


compute_clouts(heavy_elasticities_clean)
compute_vulns(heavy_elasticities_clean)

compute_clouts(light_elasticities_clean)
compute_vulns(light_elasticities_clean)

