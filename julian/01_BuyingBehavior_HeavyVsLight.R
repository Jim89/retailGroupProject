# Step 0 - load packages -----------------------------------------------------

# Step 1 - load data ---------------------------------------------------------
setwd("C:/Users/Julian/Documents/retailGroupProject/data/results")
coffee <- read.csv("buying_behaviour.csv")

# Step 2 - analyze differenes in buying behaviour (heavy vs. light)

#    The approach is as follows:
# 1. Inspecting variances and formally testing for equality using Fisher's F-test
# 2. Depending on outcome of Fisher's F-test testing for significant differences in means
#    using the independent two-samples t-test (equal or unequal variances)
# 3. Testing for equality of the distributions using Wilcoxon-Mann-Whitney test (does not assume ~N())

# Compare variance of avg_weekly_spend across heavy and light users
var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend)
var(coffee[coffee$cust_type == "Light", ]$avg_weekly_spend)

var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend, 
         coffee[coffee$cust_type == "Light", ]$avg_weekly_spend, 
         alternative = "two.sided")

t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_spend, 
       coffee[coffee$cust_type == "Light", ]$avg_weekly_spend, 
       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox.test(avg_weekly_spend ~ cust_type, data = coffee)

# Compare variance of avg_weekly_visits across heavy and light users
var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits)
var(coffee[coffee$cust_type == "Light", ]$avg_weekly_visits)

var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits, 
         coffee[coffee$cust_type == "Light", ]$avg_weekly_visits, 
         alternative = "two.sided")

t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_visits, 
       coffee[coffee$cust_type == "Light", ]$avg_weekly_visits, 
       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox.test(avg_weekly_visits ~ cust_type, data = coffee)

# Compare variance of avg_weekly_packs across heavy and light users
var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs)
var(coffee[coffee$cust_type == "Light", ]$avg_weekly_packs)

var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs, 
         coffee[coffee$cust_type == "Light", ]$avg_weekly_packs, 
         alternative = "two.sided")

t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_packs, 
       coffee[coffee$cust_type == "Light", ]$avg_weekly_packs, 
       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox.test(avg_weekly_packs ~ cust_type, data = coffee)

# Compare variance of avg_weekly_vol across heavy and light users
var(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol)
var(coffee[coffee$cust_type == "Light", ]$avg_weekly_vol)

var.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol, 
         coffee[coffee$cust_type == "Light", ]$avg_weekly_vol, 
         alternative = "two.sided")

t.test(coffee[coffee$cust_type == "Heavy", ]$avg_weekly_vol, 
       coffee[coffee$cust_type == "Light", ]$avg_weekly_vol, 
       alternative = "two.sided", var.equal = FALSE, conf.level = 0.95)

wilcox.test(avg_weekly_vol ~ cust_type, data = coffee)