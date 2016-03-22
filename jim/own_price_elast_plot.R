
diag(heavy_elasticities_clean) %>% as.data.frame() %>% 
bind_rows(diag(light_elasticities_clean) %>% as.data.frame()) %>% 
  bind_cols(data.frame(cust = c(rep("heavy", 6), rep("light", 6)))) %>% 
  bind_cols(data.frame(rep(colnames(heavy_elasticities_clean), 2))) %>% 
  setNames(c("elast", "cust", "brand")) %>% 
  ggplot(aes(x = toproper(brand), y = abs(elast))) +
  geom_bar(stat = "identity", aes(fill = cust)) +
  facet_grid(. ~ cust) +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  xlab("") +
  ylab("Own-price elasticity") +
  guides(fill = guide_legend(title = "Customer Type")) +
  theme +
  theme(axis.text.x = element_text(angle = -90)) +
  theme(strip.text = element_blank())
