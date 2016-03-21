# Step 0 - prepare environment -------------------------------------------------
# In-line function definition used in this script only
# This function takes the coffee data, filters to just one customer type (e.g.
# heavy or light), aggregates to a weekly level and then spreads the data
# in to a wide form suitable for modelling
filter_and_widen <- function(data, cust_status = 0) {
  # Filter and widen at brand level
  brand_level <- data %>% 
    filter(cust_type == cust_status) %>% 
    group_by(relweek, brand_clean) %>% 
    summarise(sales = sum(packs),
              price = mean(price),
              promo_sales_price = sum(promo_price),
              promo_sales_units = sum(promo_units)) %>% 
    mutate(promo_cost = promo_sales_price/sales,
           promo_units = promo_sales_units/sales) %>% 
    select(-promo_sales_price, -promo_sales_units) %>% 
    gather(variable, value, -(relweek:brand_clean)) %>% 
    unite(temp, brand_clean, variable, sep = "_") %>% 
    spread(temp, value) %>% 
    ungroup()
  
  colnames(brand_level) <- colnames(brand_level) %>% gsub(" ", "_", .) %>% tolower()
  
  # Calculate total sales
  total_sales <- data %>% 
    filter(cust_type == cust_status) %>% 
    group_by(relweek) %>% 
    summarise(total_sold = sum(packs))
  
  # Join together
  overall <- left_join(brand_level, total_sales)
  
  # Create brand shares
  overall <- overall %>% 
    mutate(carte_noire_share = carte_noire_sales / total_sold,
           douwe_egbert_share = douwe_egbert_sales / total_sold,
           kenco_share = kenco_sales / total_sold,
           nescafe_share = nescafe_sales / total_sold,
           other_brands_share = other_brands_sales / total_sold,
           supermarket_own_share = supermarket_own_sales / total_sold) %>% 
    select(-total_sold)
  
  return(brand_level)
}

# Step 1 - perform the spread --------------------------------------------------
# Filter and spread
heavy <- filter_and_widen(coffee_clean, "heavy")
light <- filter_and_widen(coffee_clean, "light")

# Convert missing values to 0
# heavy[is.na(heavy)] <- 0
# light[is.na(light)] <- 0

# heavy <- heavy[complete.cases(heavy), ]

for(i in 1:ncol(heavy)){
  val <- mean(heavy[,i] %>% sapply(as.numeric), na.rm = TRUE)
  heavy[is.na(heavy[,i]), i] <- val
  # print(val)
}


for(i in 1:ncol(light)){
  val <- mean(light[,i] %>% sapply(as.numeric), na.rm = TRUE)
  light[is.na(light[,i]), i] <- val
  # print(val)
}

