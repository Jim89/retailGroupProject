# Step 0 - prepare environment -------------------------------------------------

# Step 1 - create house level summary stats ------------------------------------
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


distinct_shops <- coffee_clean %>% 
  group_by(house, shop_desc_clean) %>% 
  tally() %>% 
  group_by(house) %>% 
  summarise(shops = n())

distinct_brands <- coffee_clean %>% 
  group_by(house, brand_clean) %>% 
  tally() %>% 
  group_by(house) %>% 
  summarise(brands = n())

spend_stats <- coffee_clean %>% 
  group_by(house) %>% 
  summarise(max_spend = max(netspend),
            min_spend = min(netspend))

promo_stats <- coffee_clean %>% 
  group_by(house) %>% 
  summarise(total_purchases = n(),
            promo_price = sum(promo_price),
            promo_units = sum(promo_units)) %>% 
  mutate(prop_promo_price = promo_price/total_purchases,
         prop_promo_units = promo_units/total_purchases) %>% 
  select(-promo_price, -promo_units, -total_purchases)

# Step 2 - combine in to single data set ---------------------------------------
# Set up quartiles for light/medium split
quartiles <- quantile(house_summary$avg_weekly_vol)

# Create data set
buying_behaviour <- house_summary %>% 
          left_join(distinct_shops) %>% 
          left_join(distinct_brands) %>% 
          left_join(spend_stats) %>% 
          left_join(promo_stats) %>% 
          mutate(cust_type = ifelse(avg_weekly_vol <= quartiles[2], "Light",
                                    ifelse(avg_weekly_vol >= quartiles[4], "Heavy", 
                                           "medium"))) %>% 
          filter(cust_type != "medium") 

# Write to csv
write_csv(buying_behaviour, "./data/results/buying_behaviour.csv")

# Step 3 - clean up and garbage collect ----------------------------------------
objects <- ls()
idx <- grep("clout_and_vuln_stats|buying_behaviour|coffee_clean|heavy|light|heavy_elasticities_clean|light_elasticities_clean|normalise", objects)
objects <- objects[-idx]

rm(list = objects)
rm(idx, objects)
gc()
             