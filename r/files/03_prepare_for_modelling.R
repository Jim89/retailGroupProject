# Step 0 - prepare environment -------------------------------------------------
# In-line function definition used in this script only
# This function takes the coffee data, filters to just one customer type (e.g.
# heavy or light), aggregates to a weekly level and then spreads the data
# in to a wide form suitable for modelling
filter_and_widen <- function(data, cust_status = 0) {
  data %>% 
    filter(cust_type == cust_status) %>% 
    group_by(relweek, brand_clean) %>% 
    summarise(sales = sum(packs),
              price = mean(price),
              promo_sales = sum(promo)) %>% 
    mutate(promo = promo_sales/sales) %>% 
    select(-promo_sales) %>% 
    gather(variable, value, -(relweek:brand_clean)) %>% 
    unite(temp, brand_clean, variable, sep = "_") %>% 
    spread(temp, value) %>% 
    ungroup()
}

# Step 1 - perform the spread --------------------------------------------------
# Filter and spread
heavy <- filter_and_widen(coffee_clean, "heavy")
light <- filter_and_widen(coffee_clean, "light")

# Create cleaner column names for easier selection
colnames(heavy) <- colnames(heavy) %>% gsub(" ", "_", .) %>% tolower()
colnames(light) <- colnames(light) %>% gsub(" ", "_", .) %>% tolower()



