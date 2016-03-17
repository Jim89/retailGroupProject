# Step 0 - load packages -------------------------------------------------------------------------------------

library(dplyr)
library(broom)
library(DescTools)

# Step 1 - load data -----------------------------------------------------------------------------------------

coffee <- read.csv("./data/results/buying_behaviour.csv")

# Step 2 - analyze differences in average weekly spend/visits/packs/vol in turn ------------------------------

#    The approach is as follows:
# 1. Inspecting variances and formally testing for equality using F-test
# 2. Depending on outcome of F-test testing for significant differences in means
#    using the independent two-samples t-test (equal or unequal variances)
# 3. Testing for equality of the distributions using Wilcoxon-Mann-Whitney test (does not assume ~N())

# AVERAGE WEEKLY SPEND
var_spend_heavy <- var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend)
var_spend_light <- var(coffee[coffee$cust_type == "Light", ]$avg_weekly_spend)

var_test_spend <- var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend, 
                           coffee[coffee$cust_type == "Light", ]$avg_weekly_spend, 
                           alternative = "two.sided")

mean_spend_heavy <- var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend)
mean_spend_light <- var(coffee[coffee$cust_type == "Light", ]$avg_weekly_spend)

tTest_spend <- t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend, 
                      coffee[coffee$cust_type == "Light", ]$avg_weekly_spend, 
                      alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_spend <- wilcox.test(avg_weekly_spend ~ cust_type, data = coffee)

# AVERAGE WEEKLY VISITS (for weeks in which a trip was made)
var_visits_heavy <- var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits)
var_visits_light <- var(coffee[coffee$cust_type == "Light", ]$avg_weekly_visits)

var_test_visits <- var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits, 
                            coffee[coffee$cust_type == "Light", ]$avg_weekly_visits, 
                            alternative = "two.sided")

mean_visits_heavy <- mean(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits)
mean_visits_light <- mean(coffee[coffee$cust_type == "Light", ]$avg_weekly_visits)

tTest_visits <- t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits, 
                       coffee[coffee$cust_type == "Light", ]$avg_weekly_visits, 
                       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_visits <- wilcox.test(avg_weekly_visits ~ cust_type, data = coffee)

# AVERAGE WEEKLY PACKS
var_packs_heavy <- var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs)
var_packs_light <- var(coffee[coffee$cust_type == "Light", ]$avg_weekly_packs)

var_test_packs <- var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs, 
                           coffee[coffee$cust_type == "Light", ]$avg_weekly_packs, 
                           alternative = "two.sided")

mean_packs_heavy <- mean(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs)
mean_packs_light <- mean(coffee[coffee$cust_type == "Light", ]$avg_weekly_packs)


tTest_packs <- t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs, 
                      coffee[coffee$cust_type == "Light", ]$avg_weekly_packs, 
                      alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_packs <- wilcox.test(avg_weekly_packs ~ cust_type, data = coffee)

# AVERAGE WEEKLY VOLUME
var_vol_heavy <- var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol)
var_vol_light <- var(coffee[coffee$cust_type == "Light", ]$avg_weekly_vol)

var_test_vol <- var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol, 
                         coffee[coffee$cust_type == "Light", ]$avg_weekly_vol, 
                         alternative = "two.sided")

mean_vol_heavy <- mean(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol)
mean_vol_light <- mean(coffee[coffee$cust_type == "Light", ]$avg_weekly_vol)

tTest_vol <- t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol,
                    coffee[coffee$cust_type == "Light", ]$avg_weekly_vol, 
                    alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox_vol <- wilcox.test(avg_weekly_vol ~ cust_type, data = coffee)

# Step 3 - tidy up results ---------------------------------------------------------------------------------

# Tidy up test results and bind into dataframes
var_tests    <- rbind(tidy(var_test_spend), tidy(var_test_visits), tidy(var_test_packs), tidy(var_test_vol))
t_tests      <- rbind(tidy(tTest_spend), tidy(tTest_visits), tidy(tTest_packs), tidy(tTest_vol))
wilcox_tests <- rbind(tidy(wilcox_spend), tidy(wilcox_visits), tidy(wilcox_packs), tidy(wilcox_vol))

# Add variances to var_tests
variances <- data.frame(var_spend_light, var_spend_heavy)
variances <- rbind(variances, c(var_visits_light, var_visits_heavy))
variances <- rbind(variances, c(var_packs_light, var_packs_heavy))
variances <- rbind(variances, c(var_vol_light, var_vol_heavy))

var_tests <- cbind(var_tests, variances)

# Add means to t_tests
means <- data.frame(mean_spend_light, mean_spend_heavy)
means <- rbind(means, c(mean_visits_light, mean_visits_heavy))
means <- rbind(means, c(mean_packs_light, mean_packs_heavy))
means <- rbind(means, c(mean_vol_light, mean_vol_heavy))

t_tests <- cbind(t_tests, means)

# Rename columns meaningfully and add ID ('variable')
wilcox_tests <- rename(wilcox_tests, mann.whitney.u.statistic = statistic)
var_tests    <- rename(var_tests, f.statistic = statistic)
t_tests      <- rename(t_tests, difference = estimate, mean_heavy = estimate1, mean_light = estimate2)

variables <- c("spend", "visits", "packs", "vol")
wilcox_tests <- cbind(wilcox_tests, variables)
var_tests    <- cbind(var_tests, variables)
t_tests      <- cbind(t_tests, variables)

# Step 4 - clean up, keep only 'tided up' data frames containing test results ---------------------------------------

rm(list= ls()[!(ls() %in% c('t_tests', 'var_tests', 'wilcox_tests'))])
gc(verbose = FALSE)