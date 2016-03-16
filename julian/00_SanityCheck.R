# Step 0 - load packages--------------------------------------------------------------------------------
library(dplyr)
library(funModeling)

# Step 1 - load and filter data-------------------------------------------------------------------------
setwd("C:/Users/Julian/Desktop/MScBA/Retail&Marketing/Group_Assignment")
coffee <- read.csv("InstantCoffee.csv",
                   encoding = "latin1",
                   stringsAsFactors = FALSE)

# Filter retailers
coffee <- filter(coffee, shop_desc %in% c("1TESCO",
                                          "2ASDA", 
                                          "3SAINSBURYS", 
                                          "4MORRISONS", 
                                          "7DICSOUNTERS Aldi", 
                                          "7DISCOUNTERS Lidl"))

# Filter categories
coffee <- filter(coffee, sub_cat_name %in% c("Freeze Dried",
                                             "Granules",
                                             "Micro Ground",
                                             "Decaf Freeze Dried"))

# Merge Aldi and Lidl
for (i in 1:nrow(coffee))
{
  if (coffee$shop_desc[i] == "7DISCOUNTERS Aldi" | coffee$shop_desc[i] == "7DISCOUNTERS Lidl"){
    coffee$shop_desc[i] <- "Aldi/Lidl"
  } else {
    next
  } 
}

# Step 2 - Sanity check--------------------------------------------------------------------------------
# Check for 0's and NA's
status <- df_status(coffee) # No NA's, 619 0's for NETSPEND, 3F2 -> 3rd pack recorded with NETSPEND == 0

# Function to compute repeated rows in order to identify anomalies
count_repeat <- function(data)
  {
  repeated <- data[duplicated(data) | duplicated(data, fromLast=TRUE), ]
  unique_repeated <- unique(repeated)
  l <- duplicated(repeated)
  count <- c()
  n <- 1
  for (i in 1:(length(l))){
    if(i < length(l)){
      if (l[i] == l[i + 1] & l[i] == TRUE) {n <- n + 1}
      else if (l[i] != l[i + 1] & l[i] == TRUE){
        n <- n + 1
        count <- c(count, n)
      }
      else {n <- 1}
    }
    else {
      count <- c(count, n + 1)
    }
  }
  result <- cbind(unique_repeated, count)
  return (result)
}
# There are repeated/identical rows for < 15000 households. Assumption: These are due to the fact that each row/observation
# represents only 1 pack and the repeated rows indicates that a household bought several packs in "one go"
# (this is probably sensible for most observations but not all - also, why were there 2 and 3 packs in the original/unfiltered
# data set?)

# Call function to create data frame including the number of 'repeats'
ordered_coffee <- coffee[order(coffee$HOUSE, coffee$NETSPEND, coffee$DAY, coffee$Volume), ]
repeated_rows <- count_repeat(ordered_coffee)
# Many repeated identical rows

# Compute frequencies of households
frequencies <- as.data.frame(table(coffee$HOUSE))
names(frequencies) <- c("house", "frequency")

# Function to identify quase-identical rows (i.e. only the column-value for NETSPEND differs)
unique_coffee <- unique(coffee)
unique_coffee <- unique_coffee[order(unique_coffee$relweek, unique_coffee$shop_desc, unique_coffee$total_range_name,
                                     unique_coffee$HOUSE, unique_coffee$DAY), ]

df <- data.frame()
for (r in 1:(nrow(unique_coffee) - 1)){
  same = TRUE
  for (c in 1:(ncol(unique_coffee) - 1)){
    if (unique_coffee[r, c] != unique_coffee[r + 1, c]){
      same = FALSE
      break
    }
  }
  if (same == TRUE){
    df <- rbind(df, unique_coffee[r, ])
    df <- rbind(df, unique_coffee[r + 1, ])
  }
}
anomalies <- unique(df)
nrow(anomalies) 
# 1347 anomalies, Explanation: Either explained by specific promotion type or small discrepancies which are economically insignificant
# In particular when considering the total size of the data set