# Step 0 - prepare environment -------------------------------------------------
# Function to visualise a field in the buying behaviour data split by light vs heavy 
dist_split <- function(data, field, bins = 30, type = "histogram") {
  if (type == "histogram") {
    data %>% 
      ggplot(aes_string(x = field)) +
      geom_histogram(aes(fill = cust_type), colour = "white", binwidth = bins) +
      scale_fill_brewer(type = "qual", palette = "Dark2") + 
      guides(fill = guide_legend(title = "Customer Type")) +
      facet_grid(. ~ cust_type) +
      ylab("Count") 
  } else if (type == "violin") {
    data %>% 
      ggplot(aes_string(x = "cust_type", y = field)) +
      geom_violin(aes(fill = cust_type), colour = "white") +
      scale_fill_brewer(type = "qual", palette = "Dark2") + 
      guides(fill = guide_legend(title = "Customer Type"))
      # facet_grid(. ~ cust_type) +
      
  }
}



# Step 1 - create the plots ----------------------------------------------------
# Split plot by prop promo price
bb_prop_promo_price <- dist_split(buying_behaviour, "prop_promo_price", type = "violin") +
                        ylab("Proportion sales on\n unit promotion") +
                        xlab("") +
                        theme


# Split plot by max spend
bb_max_spend <- dist_split(buying_behaviour, "max_spend", 1) + 
                xlab("Maximum spend in any single trip (£)") +
                scale_x_continuous(limits = c(0, 10)) +
                theme + 
                theme(strip.text = element_blank())

# Split plot by max spend
bb_avg_weekly_packs <- dist_split(buying_behaviour, "avg_weekly_packs", 1) + 
                        xlab("Average packs per visit") +
                        scale_x_continuous(limits = c(1, 7),
                                           breaks = seq(1, 7, 1)) +
                        theme + 
                        theme(strip.text = element_blank())


# Split plot by avg_weekly_spend
bb_avg_weekly_spend <- dist_split(buying_behaviour, "avg_weekly_spend", 2) + 
                        xlab("Average spend per visit (£)") +
                        theme +
                        theme(strip.text = element_blank())


# Split plot by avg_weekly_visits
bb_avg_weekly_visits <- dist_split(buying_behaviour, "avg_weekly_visits", 1) + 
                        xlab("Average weekly visits") +
                        theme +
                        theme(strip.text = element_blank())


# Split plot by brand loyalty
bb_brand_loyal <- dist_split(buying_behaviour, "brand_loyalty", bins = .1) +
                  xlab("Brand loyalty") +
                  theme +
                  theme(strip.text = element_blank())
  
bb_store_loyal <- dist_split(buying_behaviour, "store_loyalty", bins = .1) +
                  xlab("Store loyalty") +
                  theme +
                  theme(strip.text = element_blank())