# source data
source("./r/analysis.R")
library(magrittr)

# summarise all brands
brand <- coffee_clean %>% 
        group_by(brand_name, total_range_name) %>% 
        tally() %>% 
        rename(sales = n)

# clean up brand names to fewer categories
brand %<>% mutate(brand_clean = ifelse(sales < 5000, "Other brands", brand_name),
                  #brand_clean = ifelse(brand_name == "PL_Standard", "Supermarket own", brand_clean),
                  #brand_clean = ifelse(brand_name == "PL_Premium", "Supermarket premium", brand_clean),
                  #brand_clean = ifelse(brand_name == "PL_Value", "Supermarket value", brand_clean),
                  brand_clean = ifelse(grepl("PL_", brand_name), "Supermarket own", brand_clean),
                  brand_clean = ifelse(grepl("Nescaf", brand_name), "Nescafe", brand_clean),
                  brand_clean = gsub("\\(.*\\)", "", brand_clean)) %>% 
          arrange(brand_clean) %T>% View
          