# Step 0 - load packages --------------------------------------------------------------------------------------------------

# Step 4 - running Logistic Regression analyses: Herfindahl-index (proxy for loyalty) on cust_type dummy -----------------

mdl_brand_loy  <- glm(brand_loyalty~cust_type, family = binomial(link = "logit"), data = buying_behaviour)
mdl_store_loy  <- glm(store_loyalty~cust_type, family = binomial(link = "logit"), data = buying_behaviour)

# Step 5 - tidy up results -----------------------------------------------------------------------------------------------

tidy_brand_loy <- tidy(mdl_brand_loy)
tidy_store_loy <- tidy(mdl_store_loy)
tidy_loyalty   <- rbind(tidy_brand_loy, tidy_store_loy)

variables    <- c("Brand loyalty", "", "Store loyalty", "")
tidy_loyalty <- cbind(tidy_loyalty, variables)

# Clean up
rm(tidy_brand_loy, tidy_store_loy, variables, mdl_brand_loy, mdl_store_loy)




