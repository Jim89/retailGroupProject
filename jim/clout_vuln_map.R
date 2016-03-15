library(ggplot2)


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

clout_and_vuln_stats %>% 
  ggplot(aes(x = vuln, y = clout)) +
  geom_point(size = 5, aes(colour = brand)) +
  facet_grid(. ~ cust) +
  theme