# Step 0 - prepare env ---------------------------------------------------------


# Step 1 - prepare data --------------------------------------------------------
# Create transaction ID
trans_id <- coffee_clean %>% 
            select(relweek, day, house, shop_desc_clean) %>% 
              distinct() %>% 
              mutate(transaction_id = row_number())

# Add back to data
coffee_clean <- coffee_clean %>% left_join(trans_id)

# Find unique brands for each transaction
brands_per_id_light <- coffee_clean %>% 
                        filter(cust_type == "light") %>% 
                        select(transaction_id, brand_clean) %>% 
                        distinct()

brands_per_id_heavy <- coffee_clean %>% 
                        filter(cust_type == "heavy") %>% 
                        select(transaction_id, brand_clean) %>% 
                        distinct()

# Step 2 - reshape to co-occurence ---------------------------------------------
# Get co-occurenc matrix (may be inefficient on large data)
cooccurence_light <- brands_per_id_light %>% table() %>% crossprod()
cooccurence_heavy <- brands_per_id_heavy %>% table() %>% crossprod()

# Set diagonal to 0
diag(cooccurence_light) <- 0
diag(cooccurence_heavy) <- 0





