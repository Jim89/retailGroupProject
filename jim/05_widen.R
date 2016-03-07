# Step 0 - prepare environment -------------------------------------------------
source("./r/analysis.R")
library(tidyr)


filter_and_widen <- function(data, cust_status = 0) {
  data %>% 
    filter(heavy == cust_status) %>% 
    group_by(relweek, brand_clean) %>% 
    summarise(sales = sum(packs),
              price = mean(price),
              promo_sales = sum(promo)) %>% 
    mutate(promo = promo_sales/sales) %>% 
    select(-promo_sales) %>% 
    gather(variable, value, -(relweek:brand_clean)) %>% 
    unite(temp, brand_clean, variable, sep = "_") %>% 
    spread(temp, value)
}

heavy <- filter_and_widen(coffee_clean, 1)
light <- filter_and_widen(coffee_clean, 0)

colnames(heavy) <- colnames(heavy) %>% gsub(" ", "_", .) %>% tolower()
colnames(light) <- colnames(light) %>% gsub(" ", "_", .) %>% tolower()

