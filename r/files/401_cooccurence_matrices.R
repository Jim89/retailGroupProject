# Step 0 - prepare env ---------------------------------------------------------


# Step 1 - prepare data --------------------------------------------------------
# Create function to find unique brands for each transaction
find_brands <- function(cust){
  coffee_clean %>% 
    filter(cust_type == cust) %>% 
    select(transaction_id, brand_clean) %>% 
    distinct()
}

# Apply function for heavy and light users
brands_per_id_light <- find_brands("light")
brands_per_id_heavy <- find_brands("heavy")

# Step 2 - reshape to co-occurence ---------------------------------------------
# Get co-occurenc matrix (may be inefficient on large data)
cooccurence_light <- brands_per_id_light %>% table() %>% crossprod()
cooccurence_heavy <- brands_per_id_heavy %>% table() %>% crossprod()

# Set diagonal to 0
diag(cooccurence_light) <- 0
diag(cooccurence_heavy) <- 0

# Write to csv
cooccurence_light %>% as.data.frame %>% as_data_frame() %>% 
write_csv("./data/results/cooccurence_light.csv")

cooccurence_heavy %>% as.data.frame %>% as_data_frame() %>% 
  write_csv("./data/results/cooccurence_heavy.csv")



