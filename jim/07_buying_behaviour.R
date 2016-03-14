# Step 0 - set up environment --------------------------------------------------
library(ggbiplot)
library(dplyr)
library(ggplot2)
library(randomForest)
# library(devtools)
# install_github("vqv/ggbiplot")

source("./r/files/00_clean_and_filter.R")
source("./r/files/01_light_vs_heavy.R")
source("./r/files/02_clean_brands.R")


# Step 1 - create house level summary stats ------------------------------------
house_summary <- coffee_clean %>% 
                  group_by(relweek, house) %>% 
                  summarise(records = n(),
                            total_packs = sum(packs),
                            avg_packs = mean(packs),
                            sd_packs = sd(packs),
                            total_spend = sum(netspend),
                            avg_spend = mean(netspend),
                            sd_spend = sd(netspend),
                            total_vol = sum(volume)) %>% 
                  ungroup() %>% 
                  group_by(house) %>% 
                  summarise(avg_weekly_visits = mean(records),
                            avg_weekly_packs = mean(total_packs),
                            avg_weekly_spend = mean(total_spend),
                            avg_weekly_vol = mean(total_vol)) 


distinct_shops <- coffee_clean %>% 
                  group_by(house, shop_desc_clean) %>% 
                  tally() %>% 
                  group_by(house) %>% 
                  summarise(shops = n())

distinct_brands <- coffee_clean %>% 
                    group_by(house, brand_clean) %>% 
                    tally() %>% 
                    group_by(house) %>% 
                    summarise(brands = n())

spend_stats <- coffee_clean %>% 
                group_by(house) %>% 
                summarise(max_spend = max(netspend),
                          min_spend = min(netspend))

promo_stats <- coffee_clean %>% 
                group_by(house) %>% 
                summarise(total_purchases = n(),
                          promo_price = sum(promo_price),
                          promo_units = sum(promo_units)) %>% 
                mutate(prop_promo_price = promo_price/total_purchases,
                       prop_promo_units = promo_units/total_purchases) %>% 
                       #prop_promo_none = 1 - prop_promo_price - prop_promo_units) %>% 
                select(-promo_price, -promo_units, -total_purchases)

# Combine in to single data set
quartiles <- quantile(house_summary$avg_weekly_vol)
houses <- house_summary %>% 
          left_join(distinct_shops) %>% 
          left_join(distinct_brands) %>% 
          left_join(spend_stats) %>% 
          left_join(promo_stats) %>% 
          mutate(cust_type = ifelse(avg_weekly_vol <= quartiles[2], "Light",
                                    ifelse(avg_weekly_vol >= quartiles[4], "Heavy", 
                                           "medium"))) %>% 
          filter(cust_type != "medium") 


# Step 2 - run k-means over the data to see if clusters can be separated -------
kfit <- kmeans(houses %>% select(-house, -cust_type, -avg_weekly_vol),
               2, iter.max = 200)

houses$cluster <- kfit$cluster

# Check accuracy of k-means wrt actual labels
diag_sum <- table(houses$cluster, houses$cust_type) %>% as.matrix() %>% diag %>% sum
accuracy <- diag_sum/nrow(houses)


# Step 3 - Perform PCA to help visualise the whole data ------------------------
houses_pca <- prcomp(houses %>% select(-house, -cust_type, -cluster, -avg_weekly_vol),
                     center = TRUE,
                     scale = TRUE)

# Step 4 - Perform random forest prediction for variable importance ------------
rf <- randomForest(as.factor(cust_type) ~ ., 
                   data = houses %>% select(-house, -avg_weekly_vol),
                   importance = TRUE)

varImpPlot(rf, 
           main = "Variable importance for determining heavy vs. light users",
           type = 1)


# Step 4 - Visualise! ----------------------------------------------------------
# Set up common theme template
theme <- theme(legend.position = "bottom",
           axis.text.y = element_text(size = 16, colour = "black"),
           axis.text.x = element_text(size = 16, colour = "black"),
           legend.text = element_text(size = 16),
           legend.title = element_text(size = 16),
           title = element_text(size = 16),
           strip.text = element_text(size = 16, colour = "black"),
           strip.background = element_rect(fill = "white"),
           panel.grid.minor.x = element_blank(),
           panel.grid.major.x = element_line(colour = "grey", linetype = "dotted"),
           panel.grid.minor.y = element_line(colour = "lightgrey", linetype = "dotted"),
           panel.grid.major.y = element_line(colour = "grey", linetype = "dotted"),
           panel.margin.y = unit(0.1, units = "in"),
           panel.background = element_rect(fill = "white", colour = "lightgrey"),
           panel.border = element_rect(colour = "black", fill = NA))

## PCA plot coloured by k-means clusters
k_means_pcs <- ggbiplot(houses_pca, obs.scale = 1, var.scale = 1, 
                         groups = as.factor(houses$cluster), ellipse = TRUE, 
                         circle = F, alpha = 0.25, var.axes = F, 
                         size = 2.5) +
                scale_color_brewer(type = "qual", palette = "Dark2") + 
                guides(colour = guide_legend(title = "Customer Type")) +
                theme

# PCA plot coloured by customer type
cust_type_pca <- ggbiplot(houses_pca, obs.scale = 1, var.scale = 1, 
                           groups = houses$cust_type, ellipse = TRUE, 
                           circle = F, alpha = 0.25, var.axes = F, 
                           size = 2.5) +
                  scale_color_brewer(type = "qual", palette = "Dark2") + 
                  guides(colour = guide_legend(title = "Customer Type")) +
                  theme

# Function to visualise field split by light vs heavy 
dist_split <- function(data, field) {
  data %>% 
    ggplot(aes_string(x = field)) +
    geom_histogram(aes(fill = cust_type), colour = "white") +
    scale_fill_brewer(type = "qual", palette = "Dark2") + 
    guides(fill = guide_legend(title = "Customer Type")) +
    facet_grid(. ~ cust_type) +
    ylab("Count") +
    theme
}



# Split plot by prop promo price
dist_split(houses, "prop_promo_price") + 
  xlab("Proportion of goods bought on price promotion") 
  ggsave("./jim/images/buying_behaviour/prop_promo.svg")

# Split plot by max spend
dist_split(houses, "max_spend") + xlab("Maximum spend in any single trip (£)")
ggsave("./jim/images/buying_behaviour/max_spend.svg")

# Split plot by avg_weekly_spend
dist_split(houses, "avg_weekly_spend") + xlab("Average weekly spend (£)")
ggsave("./jim/images/buying_behaviour/avg_weekly_spend.svg")

# Split plot by avg_weekly_visits
dist_split(houses, "avg_weekly_visits") + xlab("Average weekly visits")
ggsave("./jim/images/buying_behaviour/avg_weekly_visits.svg")

# Split plot by brands
dist_split(houses, "brands") + xlab("Distinct brands purchased") +
  scale_x_continuous(breaks = seq(1, 6, 1))
ggsave("./jim/images/buying_behaviour/distinct_brands_purhcased.svg")

# Split plot by shops
dist_split(houses, "shops") + xlab("Distinct stores shopped at")
ggsave("./jim/images/buying_behaviour/distinct_stores.svg")


