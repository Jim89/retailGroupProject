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

heavy_wide <- filter_and_widen(coffee_clean, 1)
light_wide <- filter_and_widen(coffee_clean, 0)

colnames(heavy_wide) <- colnames(heavy_wide) %>% gsub(" ", "_", .) %>% tolower()
colnames(light_wide) <- colnames(light_wide) %>% gsub(" ", "_", .) %>% tolower()

