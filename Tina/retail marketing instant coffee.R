
 data<-read.csv("InstantCoffee (confidential).csv")
 head(data)
 library(dplyr)
 filter_level1<-filter(data, shop_desc=="1TESCO"| shop_desc=="3SAINSBURYS" | shop_desc=="4MORRISONS"| shop_desc=="2ASDA"| shop_desc=="7DISCOUNTERS Aldi"| 
 shop_desc=="7DISCOUNTERS Lidl")
 
 filter_level2<-filter(filter_level1, sub_cat_name=="Granules"| sub_cat_name=="Micro Ground"| sub_cat_name=="Freeze Dried")
 
new_col_v5<-filter_level2 %>% mutate(prom = ifelse(epromdesc == "No Promotion", 1, 0))

// calculate Volume per HOUSE
groupby_house_week <- group_by(new_col_v5, HOUSE,Volume)
groupby_house_week1 <- summarise(groupby_house_week, n = n())
groupby_Volume_perhouse<-summarize(groupby_house_week1,m=sum(Volume))
average_volume<-mean(groupby_Volume_perhouse$m)

// range of volume
range_volume<-range(groupby_Volume_perhouse$m)
hist(groupby_Volume_perhouse$m)
 
 
 
 
 
