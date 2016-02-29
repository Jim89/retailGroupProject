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
# coffee <- coffee %>% 
#           mutate(year = as.factor(year),
#                  day = as.factor(day),
#                  packs = as.factor(packs))
                 

# Step 2 - Create some exploratory plots ---------------------------------
# Create function to draw histogram
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
total_spend <- coffee %>% 
                group_by(relweek) %>% 
                summarise(total_spend = sum(netspend, na.rm = TRUE)) %>% 
                ggplot(aes(x = relweek, y = total_spend)) +
                geom_line(color = "steelblue") + 
                theme_minimal()
ggsave("./jim/images/fieldRelationships/spendVsWeek.svg", total_spend, 
       height = 8, width = 10)


# Examine sales over time by shop
total_spend_by_shop <- coffee %>% 
                        group_by(shop_desc,
                                 relweek) %>% 
                        summarise(total_spend = sum(netspend, na.rm = TRUE)) %>%
                        ggplot(aes(x = relweek, y = total_spend, colour = shop_desc)) +
                        geom_line(aes(colour = shop_desc), # alpha = .75,
                                  size = .75) +
                        theme_minimal()
ggsave("./jim/images/fieldRelationships/spendVsWeekByShop.svg", total_spend_by_shop,
       height = 10, width = 12)




