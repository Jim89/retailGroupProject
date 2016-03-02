# Step 0 - prepare working environment -----------------------------------------
normalise <- function(x) {
  avg <- mean(x)
  std <- sd(x)
  ans <- (x-avg)/std
  return(ans)
}  

# Step 1 - aggregate data ------------------------------------------------------
house_summary <- coffee_clean %>% 
                group_by(relweek, house) %>% 
                summarise(records = n(),
                          total_packs = sum(packs),
                          avg_packs = mean(packs),
                          sd_packs = sd(packs),
                          total_spend = sum(netspend),
                          avg_spend = mean(netspend),
                          sd_spend = sd(netspend)) %>% 
                ungroup() %>% 
                group_by(house) %>% 
                summarise(avg_weekly_visits = mean(records),
                          avg_weekly_packs = mean(total_packs),
                          avg_weekly_spend = mean(total_spend)) %>% 
                mutate(avg_weekly_spend_norm = normalise(avg_weekly_spend))

# Step 2 - classify into light vs heavy ----------------------------------------
light_vs_heavy <- house_summary %>% 
                  mutate(heavy = ifelse(avg_weekly_spend_norm > 0, 1, 0)) %>% 
                  select(house, heavy)

# Step 3 - perform the join ----------------------------------------------------
coffee_clean <- coffee_clean %>% left_join(light_vs_heavy, by = "house")

rm(house_summary)
gc(verbose = FALSE)
