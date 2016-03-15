# Step 0 - load packages -------------------------------------------------------

# Step 1 - fit k-means clustering ----------------------------------------------
kfit <- kmeans(buying_behaviour %>% select(-house, -cust_type, -avg_weekly_vol),
               2, iter.max = 200)

# Generate confusion matrix of clusters vs. customer type
conf_mat <- table(kfit$cluster, buying_behaviour$cust_type) %>% as.matrix()

# Generate k-means accuracy
accuracy <- conf_mat %>% diag() %>% sum() / conf_mat %>% sum()


# Step 1 - fit random forest ---------------------------------------------------
rf <- randomForest(as.factor(cust_type) ~ ., 
                   data = buying_behaviour %>% select(-house, -avg_weekly_vol),
                   importance = TRUE,
                   ntree = 50)

# Step 3 - remove objects and gc -----------------------------------------------
rm(kfit)
