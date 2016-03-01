# Load packages and source data
library(dplyr)
library(magrittr)
library(ggplot2)
source("./r/analysis.R")


# Define helpder function for summarisation and plotting
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

# Look at buying behaviour based on range of fields
# number of shops shopped at
house_shops <- buyingSummary(coffee_clean, "shop_desc_clean")

# Number of brands purchased
house_brands <- buyingSummary(coffee_clean, "brand_name")

# Number of ranges purchased
house_ranges <- buyingSummary(coffee_clean, "total_range_name")

# Number of product categories purchased
house_cats <- buyingSummary(coffee_clean, "sub_cat_name")

# Proportion of discounted buys
house_discounts <- coffee_clean %>% 
                    group_by(house) %>% 
                    summarise(buys = n(),
                              discounts = sum(discount),
                              prop_discount = discounts/buys)
qplot(house_discounts$prop_discount)
