# Step 0 - load packages -------------------------------------------------------
library(dplyr)
library(ggplot2)
library(GGally)

# Step 1 - read coffee -----------------------------------------------------------
setwd('/Users/Philipp/Dropbox/Imperial_College/Retail')
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
         day = as.factor(day),
         house = as.character(house))


summary(coffee)

get_histo_data <- function(data, field) {
  res <- as.data.frame(table(data[ , field]))
  colnames(res) = c(field, 'Frequency')
  return (res)
}

# get tables for relevant fields
fields <- colnames(coffee)[! colnames(coffee) %in% c('Volume', 'NETSPEND', 'HOUSE') ]
lapply(fields, function(x) get_histo_data(coffee, x))


length(unique(coffee[ , c('HOUSE', 'relweek')])[,1])




