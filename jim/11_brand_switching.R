switching_mat <- function(custs) {
data <- coffee_clean %>% 
        filter(cust_type == custs) %>% 
        arrange(house, relweek, day) %>% 
        select(relweek, house, brand_clean) %>% 
        distinct() %>% 
        group_by(house) %>% 
        mutate(prev_brand = lag(brand_clean)) %>% 
        ungroup() %>% 
        arrange(house, relweek)

mat <- table(data$brand_clean, data$prev_brand) %>% as.matrix()
}

light_switch <- switching_mat("light")
heavy_switch <- switching_mat("heavy")
