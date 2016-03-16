

v_coffee <- read.csv("./data/InstantCoffee.csv")

# 00_clean_and_filter
unique(v_coffee$shop_desc)
unique(v_coffee$sub_cat_name)
unique(v_coffee$epromdesc)
length(unique(v_coffee$HOUSE))
coffee_clean$promo_units[coffee_clean$epromdesc == 'No Promotion (Price Marked)']

# 01_light_vs_heavy
length(unique(coffee_clean$house))
coffee_clean$relweek[coffee_clean$house == '10112']
coffee_clean$relweek[coffee_clean$house == '10456']

sum(light_vs_heavy$cust_type == 'light')
sum(light_vs_heavy$cust_type == 'medium')
sum(light_vs_heavy$cust_type == 'heavy')

max(light_vs_heavy$avg_weekly_vol[light_vs_heavy$cust_type == 'light'])


# OUTPUT
# > sum(light_vs_heavy$cust_type == 'light')
# [1] 4894 (vs 4897)
# > sum(light_vs_heavy$cust_type == 'medium')
# [1] 9797 (vs 9748)
# > sum(light_vs_heavy$cust_type == 'heavy')
# [1] 4890 (vs 4936)

# 02_clean_brands
library(psych)
describe(brand$sales)
unique(brand$brand_clean)

# 03_prepare_for_modelling
cust_status = 'light'
tmp_coffee_clean <- 
coffee_clean %>% 
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

sum((tmp_coffee_clean$promo_sales_units + tmp_coffee_clean$promo_sales_price) > tmp_coffee_clean$sales)

length(unique(v_coffee$relweek))

# 04_heavy_elast
