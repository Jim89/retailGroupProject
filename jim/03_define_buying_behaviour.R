library(dplyr)
library(magrittr)
library(ggplot2)
source("./r/analysis.R")

buyingSummary <- function(data, field) {
  data <- data %>% 
    group_by_("house", field) %>% 
    tally() %>% 
    ungroup() %>% 
    group_by_("house") %>% 
    summarise(count = n())
  print(qplot(data$count))
  return(data)
}

house_shops <- buyingSummary(coffee_clean, "shop_desc_clean")
house_brands <- buyingSummary(coffee_clean, "brand_name")
house_ranges <- buyingSummary(coffee_clean, "total_range_name")
house_cats <- buyingSummary(coffee_clean, "sub_cat_name")

house_discounts <- coffee_clean %>% 
                    group_by(house) %>% 
                    summarise(buys = n(),
                              discounts = sum(discount),
                              prop_discount = discounts/buys)
qplot(house_discounts$prop_discount)
