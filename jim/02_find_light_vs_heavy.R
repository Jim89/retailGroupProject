# Step 0 - load packages and source data ---------------------------------------
library(dplyr)
library(magrittr)

source("./r/analysis.R")

summarise_by_fields <- function(data, field1, field2) {
  data %>% 
    group_by_(field1, field2) %>% 
    summarise(records = n(),
              total_packs = sum(packs),
              avg_packs = mean(packs),
              sd_packs = sd(packs),
              total_spend = sum(netspend),
              avg_spend = mean(netspend),
              sd_spend = sd(netspend))
}  


# Step 1 - find light vs heavy users -------------------------------------------
houseByWeek <- summarise_by_fields(coffee_clean, "relweek", "house")

house_summary <- houseByWeek %>% 
              group_by(house) %>% 
              summarise(avg_weekly_visits = mean(records),
                        avg_weekly_packs = mean(total_packs),
                        avg_weekly_spend = mean(total_spend))

house_summary %<>% 
  mutate(heavy = ifelse(avg_weekly_visits >= quantile(house_summary$avg_weekly_spend)[2], 1, 0))

sum(house_summary$heavy)/nrow(house_summary)
