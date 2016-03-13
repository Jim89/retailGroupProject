# Step 0 - load packages-----------------------------------------------
library(dplyr)
library(funModeling)

# Step 1 - load and filter data----------------------------------------
setwd("C:/Users/Julian/Desktop/MScBA/Retail&Marketing/Group_Assignment")
coffee <- read.csv("InstantCoffee.csv",
                   encoding = "latin1",
                   stringsAsFactors = FALSE)

# Filter retailers (Tesco, Sainsburys, Asda, Aldi, Lidl, Morrisons)
coffee <- filter(coffee, shop_desc %in% c("1TESCO",
                                          "2ASDA", 
                                          "3SAINSBURYS", 
                                          "4MORRISONS", 
                                          "7DICSOUNTERS Aldi", 
                                          "7DISCOUNTERS Lidl"))

# Filter categories (Freeze Dried, Decaf Freeze Dried, Granules, Micro Ground)
coffee <- filter(coffee, sub_cat_name %in% c("Freeze Dried",
                                             "Granules",
                                             "Micro Ground",
                                             "Decaf Freeze Dried"))

# Merge Aldi and Lidl
for (i in 1:nrow(coffee))
{
  if (coffee$shop_desc[i] == "7DISCOUNTERS Aldi" | coffee$shop_desc[i] == "7DISCOUNTERS Lidl")
  {coffee$shop_desc[i] <- "Aldi/Lidl"}
  else
  {next} 
}

# Step 2 - Sanity check----------------------------------------------
# Check for 0's and NA's
status <- df_status(coffee)
# No NA's, 619 0's for NETSPEND, All of them Promotion = 3F2 -> 3rd package recorded with NETSPEND = 0

# Function to compute repeated rows in order to identify anomalies and cause a little bit of confusion
Count_Repeat <- function(data)
  {
  repeated <- data[duplicated(data) | duplicated(data, fromLast=TRUE), ]
  unique_repeated <- unique(repeated)
  l <- duplicated(repeated)
  count <- c()
  n <- 1
  error <- 0
  for (i in 1:(length(l))){
    if(i < length(l))
    {
      if (l[i] == l[i + 1] & l[i] == TRUE)
      {n <- n+1}
      else if (l[i] != l[i + 1] & l[i] == TRUE)
      {
        n <- n + 1
        count <- c(count, n)
      }
      else{n <- 1}
    }
    else{count <- c(count, n + 1)}
  }
  count_repeat <- cbind(unique_repeated, count)
  return (count_repeat)
}

# Call function to create data frame including the number of 'repeats'
ordered_coffee <- coffee[order(coffee$HOUSE, coffee$NETSPEND), ]
count_repeat <- Count_Repeat(ordered_coffee)

# Compute frequencies of households
frequencies <- as.data.frame(table(coffee$HOUSE))
names(frequencies) <- c("house", "frequency") # rename columns
# ~10 Households which went to the shops >= 99 times