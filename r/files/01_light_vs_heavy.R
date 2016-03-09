# Step 0 - prepare working environment -----------------------------------------


# Step 1 - aggregate data ------------------------------------------------------
house_summary <- coffee_clean %>% 
                group_by(relweek, house) %>% 
                summarise(records = n(),
                          total_packs = sum(packs),
                          avg_packs = mean(packs),
                          sd_packs = sd(packs),
                          total_spend = sum(netspend),
                          avg_spend = mean(netspend),
                          sd_spend = sd(netspend),
                          total_vol = sum(volume)) %>% 
                ungroup() %>% 
                group_by(house) %>% 
                summarise(avg_weekly_visits = mean(records),
                          avg_weekly_packs = mean(total_packs),
                          avg_weekly_spend = mean(total_spend),
                          avg_weekly_vol = mean(total_vol)) 

# Step 2 - classify into light vs heavy ----------------------------------------
quartiles <- quantile(house_summary$avg_weekly_vol)
light_vs_heavy <- house_summary %>% 
                  mutate(cust_type = ifelse(avg_weekly_vol <= quartiles[2], "light",
                                      ifelse(avg_weekly_vol >= quartiles[4], "heavy", 
                                             "medium"))) %>% 
                  select(house, cust_type)

# Step 3 - perform the join ----------------------------------------------------
coffee_clean <- coffee_clean %>% left_join(light_vs_heavy, by = "house")

rm(house_summary, light_vs_heavy)
gc(verbose = FALSE)
