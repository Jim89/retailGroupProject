# Step 0 - prep env ------------------------------------------------------------

# Step 1 - summarise data ------------------------------------------------------
brands_purchased <- coffee_clean %>% 
                    filter(cust_type != "medium") %>% 
                    group_by(cust_type, brand_clean) %>% 
                    tally() %>% 
                    spread(cust_type, n)

brands_purchased <- brands_purchased %>% 
                    mutate(heavy_prop = heavy / sum(brands_purchased$heavy),
                           light_prop = light / sum(brands_purchased$light))

# Step 2 - visualise -----------------------------------------------------------
brands_purchased %>% 
  select(-heavy, -light) %>% 
  separate(brand_clean, into = c("brand_clean", "null"), sep = " ") %>% 
  select(-null) %>% 
  gather(cust, purchases, -brand_clean) %>% 
  mutate(cust = ifelse(cust == "heavy_prop", "Heavy", "Light")) %>% 
  ggplot(aes(x = brand_clean, y = purchases)) +
  geom_bar(stat = "identity", aes(fill = cust)) +
  scale_fill_brewer(type = "qual", palette = "Dark2") + 
  guides(fill = guide_legend(title = "Customer Type")) +
  facet_grid(cust ~ .) +
  xlab("Brand") +
  ylab("Proportion of purchases") +
  theme +
  theme(strip.text = element_blank())