library(dplyr)
library(ggplot2)
source("./r/files/00_clean_and_filter.R")
source("./r/files/01_light_vs_heavy.R")
source("./r/files/02_clean_brands.R")

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
                          min_spend = min(netspend),
                          avg_spend = mean(netspend))

promo_stats <- coffee_clean %>% 
                group_by(house) %>% 
                summarise(total_purchases = n(),
                          promo_price = sum(promo_price),
                          promo_units = sum(promo_units)) %>% 
                mutate(prop_promo_price = promo_price/total_purchases,
                       prop_promo_units = promo_units/total_purchases) %>% 
                       #prop_promo_none = 1 - prop_promo_price - prop_promo_units) %>% 
                select(-promo_price, -promo_units, -total_purchases)

# Combine
quartiles <- quantile(house_summary$avg_weekly_vol)
houses <- house_summary %>% 
          left_join(distinct_shops) %>% 
          left_join(distinct_brands) %>% 
          left_join(spend_stats) %>% 
          left_join(promo_stats) %>% 
          mutate(cust_type = ifelse(avg_weekly_vol <= quartiles[2], "light",
                                    ifelse(avg_weekly_vol >= quartiles[4], "heavy", 
                                           "medium"))) %>% 
          filter(cust_type != "medium")

kfit <- kmeans(houses[, -13], 2, iter.max = 30)

houses$cluster <- kfit$cluster

table(houses$cust_type, houses$cluster)

houses %>% 
  rowwise() %>% 
  mutate(avg_prop = mean(prop_promo_price, prop_promo_units)) %>% 
ggplot(aes(x = avg_weekly_spend, y = avg_prop)) +
  geom_point(aes(shape = as.factor(cluster), colour = as.factor(cust_type)))
  
