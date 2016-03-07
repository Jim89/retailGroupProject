data<-read.csv("InstantCoffee (confidential).csv")
head(data)
 library(dplyr)
 filtere_level1<-filter(data, shop_desc=="1TESCO"| shop_desc=="3SAINSBURYS" | shop_desc=="4MORRISONS"| shop_desc=="2ASDA"| shop_desc=="7DISCOUNTERS Aldi"| 
 shop_desc=="7DISCOUNTERS Lidl")
 
 filtere_level2<-filter(filtere_level1, sub_cat_name=="Granules"| sub_cat_name=="Micro Ground"| sub_cat_name=="Freeze Dried")
 
 
 