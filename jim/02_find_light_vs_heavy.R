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

normalise <- function(x) {
  avg <- mean(x)
  std <- sd(x)
  ans <- (x-avg)/std
  return(ans)
}  


# Step 1 - find light vs heavy users -------------------------------------------
houseByWeek <- summarise_by_fields(coffee_clean, "relweek", "house")

house_summary <- houseByWeek %>% 
              group_by(house) %>% 
              summarise(avg_weekly_visits = mean(records),
                        avg_weekly_packs = mean(total_packs),
                        avg_weekly_spend = mean(total_spend)) %>% 
              mutate(avg_weekly_spend_norm = normalise(avg_weekly_spend))

house_summary %<>% 
  mutate(heavy = ifelse(avg_weekly_visits >= quantile(house_summary$avg_weekly_spend,
                                                      probs = seq(0, 1, 0.1))[2], 1, 0),
         heavy2 = ifelse(avg_weekly_spend_norm > 0, 1, 0))

sum(house_summary$heavy)/nrow(house_summary)
sum(house_summary$heavy2)/nrow(house_summary)
