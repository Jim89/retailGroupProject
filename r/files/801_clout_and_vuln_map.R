# Step 0 - load packages -------------------------------------------------------

# Step 1 - Create base plot ----------------------------------------------------
clout_and_vuln_map <- clout_and_vuln_stats %>%
                      mutate(brand = toproper(brand),
                             cust = toproper(cust)) %>% 
                      ggplot(aes(x = vuln, y = clout)) +
                      geom_point(size = 7.5, aes(colour = brand), alpha = .75) +
                      scale_color_brewer(type = "qual", palette = "Dark2") + 
                      facet_grid( . ~ cust) +
                      xlab("Vulnerability") +
                      ylab("Clout") +
                      guides(colour = guide_legend(title = "Brand")) +
                      geom_hline(yintercept = 0,
                                 colour = "black", linetype = "dotted") +
                      geom_vline(xintercept = 0,
                                 colour = "black", linetype = "dotted") +
                      theme

