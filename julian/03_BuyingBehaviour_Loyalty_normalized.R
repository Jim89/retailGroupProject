library(dplyr)

hh_brand_breakdown <- read.csv("./julian/hh_brand_breakdown.csv")
hh_store_breakdown <- read.csv("./julian/hh_store_breakdown.csv")
buying.behaviour <- read.csv("./data/results/buying_behaviour.csv")

norm_herfindahl <- function(x){
  x <- as.numeric(unlist(x))
  l <- length(x)
  t <- sum(x)
  if (l > 1) {
    inter <- x/t
    x <- ((t(inter)%*%inter)-1/l)/(1-1/l)
  }
  else{
    x <- 1
  }
  x
}

hh_herf_brand <- sapply(split(hh_brand_breakdown[3],hh_brand_breakdown$house),norm_herfindahl)
hh_herf_brand <- data.frame(hh_herf_brand)

hh_herf_store <- sapply(split(hh_store_breakdown[3],hh_store_breakdown$house),norm_herfindahl)
hh_herf_store <- data.frame(hh_herf_store)

hh_herf_brand$house <- as.numeric(rownames(hh_herf_brand))
hh_herf_store$house <- as.numeric(rownames(hh_herf_store))
buying.behaviour <- left_join(buying.behaviour, hh_herf_brand)
buying.behaviour <- left_join(buying.behaviour, hh_herf_store)

mdl_brloy  <- glm(hh_herf_brand~cust_type, family = binomial(link = "logit"), data = buying.behaviour)
mdl_strloy <- glm(hh_herf_store~cust_type, family = binomial(link = "logit"), data = buying.behaviour)

summary(mdl_brloy)
summary(mdl_strloy)
