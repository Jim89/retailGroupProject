# Step 0 - load packages --------------------------------------------------------------------------------------------------

library(dplyr)
library(broom)
library(DescTools)

# Step 1 - load data ------------------------------------------------------------------------------------------------------

buying.behaviour <- read.csv("./data/results/buying_behaviour.csv")
brand_breakdown  <- read.csv("./julian/hh_brand_breakdown.csv")
store_breakdown  <- read.csv("./julian/hh_store_breakdown.csv")

brand_breakdown$X <- NULL
store_breakdown$X <- NULL
store_breakdown   <- rename(store_breakdown, store_count = brand_count)

# Step 2 - construct store & brand loyalty measures -----------------------------------------------------------------------

brandL <- group_by(brand_breakdown, house)
brandL <- summarise(brandL, Herfindahl_brand = Herfindahl(brand_count))

storeL <- group_by(store_breakdown, house)
storeL <- summarise(storeL, Herfindahl_store = Herfindahl(store_count))

# Step 3 - joining herfindahl with Buying-behaviour.csv

coffee <- left_join(buying.behaviour, brandL)
coffee <- left_join(buying.behaviour, storeL)

# Step 4 - running Logistic Regression analyses: Herfindahl-index (proxy for loyalty) on cust_type dummy ----------------

mdl_brand_loy  <- glm(Herfindahl_brand~cust_type, family = binomial(link = "logit"), data = buying.behaviour)
mdl_store_loy <- glm(Herfindahl_store~cust_type, family = binomial(link = "logit"), data = buying.behaviour)

# Step 5 - tidy up results ------------------------------------------------------------------------------------------------

tidy_brand_loy <- tidy(mdl_brloy)
tidy_store_loy <- tidy(mdl_strloy)
tidy_loyalty <- rbind(tidy_brand_loy, tidy_store_loy)

variables <- c("brand_loyalty", "brand_loyalty", "store_loyalty", "store_loyalty")
tidy_loyalty <- cbind(tidy_loyalty, variables)

# Step 6 - clean up, keep only 'tided up' dataframes containing test results ----------------------------------------------

rm(list= ls()[!(ls() %in% c('tidy_loyalty'))])
gc(verbose = FALSE)