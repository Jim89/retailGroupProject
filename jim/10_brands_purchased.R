# Step 0 - prep env ------------------------------------------------------------

# Step 1 - summarise data ------------------------------------------------------
brands_purchased <- coffee_clean %>% 
                    filter(cust_type != "medium") %>% 
                    group_by(cust_type, brand_clean) %>% 
                    tally() %>% 
                    spread(cust_type, n)