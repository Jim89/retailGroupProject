# Step 0 - prep env ------------------------------------------------------------
# Create function that will calculate switching matrix from coffee data
switching_mat <- function(custs) {
# Summarise purchases per household over time
data <- coffee_clean %>% 
        filter(cust_type == custs) %>% 
        arrange(house, relweek, day) %>% 
        select(relweek, house, brand_clean) %>% 
        distinct() %>% 
        group_by(house) %>% 
        mutate(prev_brand = lag(brand_clean)) %>% 
        ungroup() %>% 
        arrange(house, relweek)

# Create switching matrix
mat <- table(data$brand_clean, data$prev_brand) 

# Return switching matrix
return(mat)
}

# Step 1 - Generate switching matrices -----------------------------------------
light_switch <- switching_mat("light")
heavy_switch <- switching_mat("heavy")

# Write to CSV
light_switch %>% as.data.frame() %>% write_csv("./data/results/light_switch.csv")
heavy_switch %>% as.data.frame() %>% write_csv("./data/results/heavy_switch.csv")



