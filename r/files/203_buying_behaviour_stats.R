# Step 1 - load packages -------------------------------------------------------


# Step 2 - analyze differences in average weekly spend/visits/packs/vol in turn 

#    The approach is as follows:
# 1. Inspecting variances and formally testing for equality using F-test
# 2. Depending on outcome of F-test testing for significant differences in means
#    using the independent two-samples t-test (equal or unequal variances)
# 3. Testing for equality of the distributions using Wilcoxon-Mann-Whitney test (does not assume ~N())

# AVERAGE WEEKLY SPEND ------------------------------------------
var_spend_heavy <- var(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_spend)
var_spend_light <- var(buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_spend)

var_test_spend <- var.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_spend, 
                           buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_spend, 
                           alternative = "two.sided")

tTest_spend <- t.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_spend, 
                      buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_spend, 
                      alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_spend <- wilcox.test(avg_weekly_spend ~ cust_type, data = buying_behaviour)

# ------------------------------------------AVERAGE WEEKLY VISITS --------------
var_visits_heavy <- var(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_visits)
var_visits_light <- var(buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_visits)

var_test_visits <- var.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_visits, 
                            buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_visits, 
                            alternative = "two.sided")

tTest_visits <- t.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_visits, 
                       buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_visits, 
                       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_visits <- wilcox.test(avg_weekly_visits ~ cust_type, data = buying_behaviour)

# ------------------------------------------AVERAGE WEEKLY PACKS ---------------
var_packs_heavy <- var(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_packs)
var_packs_light <- var(buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_packs)

var_test_packs <- var.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_packs, 
                           buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_packs, 
                           alternative = "two.sided")

tTest_packs <- t.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_packs, 
                      buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_packs, 
                      alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_packs <- wilcox.test(avg_weekly_packs ~ cust_type, data = buying_behaviour)

# ------------------------------------------AVERAGE WEEKLY VOLUME --------------
var_vol_heavy <- var(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_vol)
var_vol_light <- var(buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_vol)

var_test_vol <- var.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_vol, 
                         buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_vol, 
                         alternative = "two.sided")

tTest_vol <- t.test(buying_behaviour[buying_behaviour$cust_type == "Heavy", ]$avg_weekly_vol,
                    buying_behaviour[buying_behaviour$cust_type == "Light", ]$avg_weekly_vol, 
                    alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_vol <- wilcox.test(avg_weekly_vol ~ cust_type, data = buying_behaviour)

# Step 3 - tidy up results -----------------------------------------------------
# Tidy up test results and bind into dataframes --------------------------------
var_tests    <- rbind(tidy(var_test_spend), tidy(var_test_visits), tidy(var_test_packs), tidy(var_test_vol))
t_tests      <- rbind(tidy(tTest_spend), tidy(tTest_visits), tidy(tTest_packs), tidy(tTest_vol))
wilcox_tests <- rbind(tidy(wilcox_spend), tidy(wilcox_visits), tidy(wilcox_packs), tidy(wilcox_vol))

# Add variances to var_tests ---------------------------------------------------
variances <- data.frame(var_spend_light, var_spend_heavy)
variances <- rbind(variances, c(var_visits_light, var_visits_heavy))
variances <- rbind(variances, c(var_packs_light, var_packs_heavy))
variances <- rbind(variances, c(var_vol_light, var_vol_heavy))

var_tests <- cbind(var_tests, variances)

# Rename columns meaningfully and add ID (=variable) ---------------------------
wilcox_tests       <- rename(wilcox_tests, mann.whitney.u.statistic = statistic)
t_tests            <- rename(t_tests, difference = estimate, mean_heavy = estimate1, 
                             mean_light = estimate2, df = parameter)
var_tests          <- rename(var_tests, f.statistic = statistic, var_light = var_spend_light, 
                             var_heavy = var_spend_heavy)
var_tests$estimate <- NULL

variables    <- c("Average spend per visit", 
                  "Average weekly visits", 
                  "Average packs per visit", 
                  "Average volume per visit")
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
