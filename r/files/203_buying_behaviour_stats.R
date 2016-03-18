# Step 0 - prep env ------------------------------------------------------------
# Function to compute variances
get_var <- function(field, custs) {
  buying_behaviour %>% 
    filter(cust_type == custs) %>% 
    select_(field) %>% 
    var()
}


# Function to perform variance test
var_test <- function(field) {
  var_heavy <- buying_behaviour %>% 
                filter(cust_type == "Heavy") %>% 
                select_(field) %>% sapply(as.numeric)
  
  var_light <- buying_behaviour %>% 
                filter(cust_type == "Light") %>% 
                select_(field) %>% sapply(as.numeric)
  
  var_test <- var.test(var_heavy, var_light, alternative = "two.sided") %>% tidy()
  
  return(var_test)
}

# Function to perform t-test
t_test <- function(field) {
  mean_heavy <- buying_behaviour %>% 
                filter(cust_type == "Heavy") %>% 
                select_(field) %>% sapply(as.numeric)
  
  mean_light <- buying_behaviour %>% 
                filter(cust_type == "Light") %>% 
                select_(field) %>% sapply(as.numeric)
  
  t_test <- t.test(mean_heavy, mean_light, alternative = "two.sided", 
                   var.equal = FALSE, conf.level = 0.95) %>% tidy()
  
  return(t_test)
}

# Function to perform Man-Whitney test
wicox <- function(field) {
  wilcox.test(eval(parse(text = field)) ~ cust_type, data = buying_behaviour) %>% 
    tidy()
}

# Create table of variances for each field
variances <- data_frame(var_light = c(get_var("avg_weekly_spend", "Light"),
                                      get_var("avg_weekly_packs", "Light"),
                                      get_var("avg_weekly_visits", "Light"),
                                      get_var("store_loyalty", "Light"),
                                      get_var("brand_loyalty", "Light"),
                                      get_var("max_spend", "Light"),
                                      get_var("min_spend", "Light"),
                                      get_var("prop_promo_price", "Light"),
                                      get_var("prop_promo_units", "Light")),
                        
                        var_heavy = c(get_var("avg_weekly_spend", "Heavy"),
                                      get_var("avg_weekly_packs", "Heavy"),
                                      get_var("avg_weekly_visits", "Heavy"),
                                      get_var("store_loyalty", "Heavy"),
                                      get_var("brand_loyalty", "Heavy"),
                                      get_var("max_spend", "Heavy"),
                                      get_var("min_spend", "Heavy"),
                                      get_var("prop_promo_price", "Heavy"),
                                      get_var("prop_promo_units", "Heavy")))

# Step 1 - Compute statistics for each field -----------------------------------
# Avg weekly spend
var_test_spend <- var_test("avg_weekly_spend")
tTest_spend <- t_test("avg_weekly_spend")
wilcox_spend <- wicox("avg_weekly_spend")

# Avg. weekly packs
var_test_packs <- var_test("avg_weekly_packs")
tTest_packs <- t_test("avg_weekly_packs")
wilcox_packs <- wicox("avg_weekly_packs")

# Avg. weekly visits
var_test_visits <- var_test("avg_weekly_visits")
tTest_visits <- t_test("avg_weekly_visits")
wilcox_visits <- wicox("avg_weekly_visits")

# Store loyalty
var_test_store_loy <- var_test("store_loyalty")
tTest_store_loy <- t_test("store_loyalty")
wilcox_store_loy <- wicox("store_loyalty")

# Brand loyalty
var_test_brand_loy <- var_test("brand_loyalty")
tTest_brand_loy <- t_test("brand_loyalty")
wilcox_brand_loy <- wicox("brand_loyalty")

# Max spend
var_test_max <- var_test("max_spend")
tTest_max <- t_test("max_spend")
wilcox_max <- wicox("max_spend")

# min spend
var_test_min <- var_test("min_spend")
tTest_min <- t_test("min_spend")
wilcox_min <- wicox("min_spend")

# Promo price
var_test_promo_price <- var_test("prop_promo_price")
tTest_promo_price <- t_test("prop_promo_price")
wilcox_promo_price <- wicox("prop_promo_price")

# Promo units
var_test_promo_units <- var_test("prop_promo_units")
tTest_promo_units <- t_test("prop_promo_units")
wilcox_promo_units <- wicox("prop_promo_units")


# Step 3 - tidy up results -----------------------------------------------------
var_tests    <- bind_rows(var_test_spend, var_test_packs, var_test_visits,
                          var_test_store_loy, 
                          var_test_brand_loy, var_test_max, var_test_min,
                          var_test_promo_price, var_test_promo_units)

t_tests <- bind_rows(tTest_spend, tTest_packs, tTest_visits, tTest_store_loy, 
                      tTest_brand_loy, tTest_max, tTest_min,
                      tTest_promo_price, tTest_promo_units)


wilcox_tests <- bind_rows(wilcox_spend, wilcox_packs, wilcox_visits,
                          wilcox_store_loy, 
                          wilcox_brand_loy, wilcox_max, wilcox_min,
                          wilcox_promo_price, wilcox_promo_units)



# Rename columns 
wilcox_tests <- wilcox_tests %>% rename(mann.whitney.u.statistic = statistic)

t_tests <- t_tests %>% rename(difference = estimate, 
                              mean_heavy = estimate1, 
                              mean_light = estimate2, df = parameter)


var_tests <- var_tests %>% rename(f.statistic = statistic) %>% 
                            select(-estimate) %>% 
                            bind_cols(variances)


# Create field identifiers
variables    <- c("Average spend per visit", 
                  "Average packs per visit",
                  "Average weekly visits", 
                  "Store loyalty",
                  "Brand loyalty",
                  "Max spend",
                  "Min spend",
                  "Prop. price promotion",
                  "Prop. unit promotion")


wilcox_tests <- cbind(wilcox_tests, variables)
var_tests    <- cbind(var_tests, variables)
t_tests      <- cbind(t_tests, variables)


rm(list= ls()[!(ls() %in% c('t_tests', 'var_tests', 'wilcox_tests', 
                            'coffee_clean', 'buying_behaviour',
                            'clout_and_vuln_stats', 'heavy', 'light',
                            'heavy_elasticities_clean', "light_elasticities_clean",
                            "tidy_loyalty", "accuracy", "conf_mat", "rf",
                            "normalise", "toproper"))])
gc(verbose = FALSE)
