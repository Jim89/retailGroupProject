# Step 0 - prepare environment -------------------------------------------------
source("./r/analysis.R")


# Step 1 - widen
library(tidyr)

# Agggregate
coffee_aggregated <- coffee_clean %>% 
  group_by(relweek, heavy, brand_clean) %>% 
  summarise(packs = sum(packs),
            avg_price = mean(price))

coffee_aggregated %>% 
  filter(heavy == 0) %>% 
  select(-heavy, - packs) %>% 
  spread(brand_clean, avg_price) %>% View

filter_and_summarise <- function(data, cust_status = 0) {
  data %>% 
    filter(heavy == cust_status) %>% 
    group_by(relweek, brand_clean) %>% 
    summarise(sales = sum(packs),
              price = mean(price),
              promo_sales = sum(promo)) %>% 
    mutate(prop_promo = promo_sales / sales)
}

heavy <- filter_and_summarise(coffee_clean, cust_status = 1)
