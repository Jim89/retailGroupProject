# Step 0 - load packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(GGally)

# Step 1 - read coffee -----------------------------------------------------------
coffee <- read.csv("./data/InstantCoffee.csv", 
                   encoding = "latin1", 
                   stringsAsFactors = FALSE) %>% 
          as_data_frame()

colnames(coffee) <- coffee %>% colnames() %>% tolower()

rows <- nrow(coffee)
complete_rows <- complete.cases(coffee) %>% sum

cols <- ncol(coffee)
complete_cols <- apply(apply(coffee, 2, complete.cases), 2, sum)

# Convert certain fields to factor
coffee <- coffee %>% 
          mutate(year = as.factor(year),
                 day = as.factor(day))

# Step 2 - Simple summaries ----------------------------------------------------
byWeek <- coffee %>% 
            group_by(relweek) %>% 
            summarise(total_packs = sum(packs),
                      avg_packs = mean(packs),
                      sd_packs = sd(packs),
                      total_spend = sum(netspend),
                      avg_spend = mean(netspend),
                      sd_spend = sd(netspend))

byDay <- coffee %>% 
          group_by(day) %>% 
          summarise(total_packs = sum(packs),
                    avg_packs = mean(packs),
                    sd_packs = sd(packs),
                    total_spend = sum(netspend),
                    avg_spend = mean(netspend),
                    sd_spend = sd(netspend))

byShop <- coffee %>% 
          group_by(shop_desc) %>% 
          summarise(total_packs = sum(packs),
                    avg_packs = mean(packs),
                    sd_packs = sd(packs),
                    total_spend = sum(netspend),
                    avg_spend = mean(netspend),
                    sd_spend = sd(netspend))

byShopbyWeek <- coffee %>% 
                group_by(shop_desc,
                         relweek) %>% 
                summarise(total_spend_shop = sum(netspend, na.rm = TRUE)) 

bySubCat <- coffee %>% 
            group_by(sub_cat_name) %>% 
            summarise(total_packs = sum(packs),
                      avg_packs = mean(packs),
                      sd_packs = sd(packs),
                      total_spend = sum(netspend),
                      avg_spend = mean(netspend),
                      sd_spend = sd(netspend))

# Step 3 - Create some exploratory plots ---------------------------------------
# Create function to draw histogram/count plots for all fields
gg_hist <- function(data, field){
  is_num <- is.numeric(data[ , field])
  name <- paste0("./jim/images/fields/", field, ".svg")
  if (is_num == TRUE) {
    plot <- ggplot(data, aes_string(x = field)) +
      geom_histogram(fill = "steelblue", colour = "white") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -90))
  } else {
    plot <- ggplot(data, aes_string(x = field)) +
      geom_bar(fill = "steelblue", colour = "white") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = -90))
  }
  # plot <- ggplotly(plot)
  ggsave(file = name, plot = plot, width = 10, height = 8)
}  

columns <- colnames(coffee)
lapply(columns, function(x) gg_hist(coffee, x))


# Examine total sales over time
total_spend <- perWeek %>% 
                ggplot(aes(x = relweek, y = total_spend)) +
                geom_line(color = "steelblue") + 
                theme_minimal()
ggsave("./jim/images/fieldRelationships/spendVsWeek.svg", total_spend, 
       height = 8, width = 10)


## Develop function to create sales over time plots by various fields
sales_over_time_by <- function(data, field) {
  name <- paste0("./jim/images/fieldsOverTime/sales_by_", field, ".svg")
  plot <- data %>% 
    group_by_(field, "relweek") %>% 
    summarise(total_spend = sum(netspend)) %>% 
    ggplot(aes_string(x = "relweek", y = "total_spend", colour = field)) +
    geom_line(aes_string(colour = field), size = 0.75) +
    theme_minimal()
  ggsave(name, plot, height = 8, width = 10)
}


# Function to show proportion of sales by field over time
prop_sales_over_time_by <- function(data, field, data2 = byWeek) {
  name <- paste0("./jim/images/shares/prop_sales_by_", field, ".svg")
  data1 <- data %>% 
            group_by_("relweek", field) %>% 
            summarise(total_sales = sum(netspend, na.rm = TRUE))
  plot <- left_join(data1, byWeek) %>% 
          mutate(prop = total_sales / total_spend) %>% 
          ggplot(aes_string(x = "relweek", y = "prop", fill = field)) +
          geom_area(aes_string(fill = field), colour = "ghostwhite") +
          theme_minimal()
  ggsave(name, plot, height = 8, width = 10)
}

# Set up list of fields
fields <- c("shop_desc", "sub_cat_name", "day") 

# Create plots
lapply(fields, function(x) sales_over_time_by(coffee, x))
lapply(fields, function(x) prop_sales_over_time_by(coffee, x))




