
data<-light
smp_size<-floor(nrow(data)*0.75)
set.seed(123)
train_ind<-sample(seq_len(nrow(data)),size=smp_size)
train<-data[train_ind, ]
test<-data[-train_ind, ]


# semi log model light

carte <- lm(log(carte_noire_sales) ~ ., data = train)
 douwe <- lm(log(douwe_egbert_sales) ~ ., data = train)
 kenco <- lm(log(kenco_sales)~ ., data = train)
 nesca <- lm(log(nescafe_sales) ~ ., data = train)
 other <- lm(log(other_brands_sales) ~ ., data = train)
 super <- lm(log(supermarket_own_sales) ~ ., data = train)
 

  # elasticity
  
  carte_el_carte<-mean(train$carte_noire_price)*summary(carte)$coefficients[3]
 douwe_el_carte<-mean(train$douwe_egbert_price)*summary(carte)$coefficients[6]
  kenco_el_carte<-mean(train$kenco_price)*summary(carte)$coefficients[10]
 nesca_el_carte <-mean(train$nescafe_price)*summary(carte)$coefficients[14]
 other_el_carte <-mean(train$other_brands_price)*summary(carte)$coefficients[18]
 super_el_carte<-mean(train$supermarket_own_price)*summary(carte)$coefficients[22]
  
  
  carte_el_douwe<-mean(train$carte_noire_price)*summary(douwe)$coefficients[3]
 douwe_el_douwe<-mean(train$douwe_egbert_price)*summary(douwe)$coefficients[7]
  kenco_el_douwe<-mean(train$kenco_price)*summary(douwe)$coefficients[10]
 nesca_el_douwe <-mean(train$nescafe_price)*summary(douwe)$coefficients[14]
 other_el_douwe <-mean(train$other_brands_price)*summary(douwe)$coefficients[18]
 super_el_douwe<-mean(train$supermarket_own_price)*summary(douwe)$coefficients[22]
  
  
  carte_el_kenco<-mean(train$carte_noire_price)*summary(kenco)$coefficients[3]
 douwe_el_kenco<-mean(train$douwe_egbert_price)*summary(kenco)$coefficients[7]
  kenco_el_kenco<-mean(train$kenco_price)*summary(kenco)$coefficients[11]
 nesca_el_kenco <-mean(train$nescafe_price)*summary(kenco)$coefficients[14]
 other_el_kenco <-mean(train$other_brands_price)*summary(kenco)$coefficients[18]
 super_el_kenco<-mean(train$supermarket_own_price)*summary(kenco)$coefficients[22]
  
  carte_el_nesca<-mean(train$carte_noire_price)*summary(nesca)$coefficients[3]
 douwe_el_nesca<-mean(train$douwe_egbert_price)*summary(nesca)$coefficients[7]
  kenco_el_nesca<-mean(train$kenco_price)*summary(nesca)$coefficients[11]
 nesca_el_nesca <-mean(train$nescafe_price)*summary(nesca)$coefficients[15]
 other_el_nesca <-mean(train$other_brands_price)*summary(nesca)$coefficients[18]
 super_el_nesca<-mean(train$supermarket_own_price)*summary(nesca)$coefficients[22]
 
  carte_el_other<-mean(train$carte_noire_price)*summary(other)$coefficients[3]
 douwe_el_other<-mean(train$douwe_egbert_price)*summary(other)$coefficients[7]
  kenco_el_other<-mean(train$kenco_price)*summary(other)$coefficients[11]
 nesca_el_other <-mean(train$nescafe_price)*summary(other)$coefficients[15]
 other_el_other <-mean(train$other_brands_price)*summary(other)$coefficients[19]
 super_el_other<-mean(train$supermarket_own_price)*summary(other)$coefficients[22]
  
    carte_el_super<-mean(train$carte_noire_price)*summary(super)$coefficients[3]
 douwe_el_super<-mean(train$douwe_egbert_price)*summary(super)$coefficients[7]
  kenco_el_super<-mean(train$kenco_price)*summary(super)$coefficients[11]
 nesca_el_super <-mean(train$nescafe_price)*summary(super)$coefficients[15]
 other_el_super <-mean(train$other_brands_price)*summary(super)$coefficients[19]
 super_el_super<-mean(train$supermarket_own_price)*summary(super)$coefficients[23]
 
 line1<-c(carte_el_carte, douwe_el_carte,  kenco_el_carte, nesca_el_carte, other_el_carte, super_el_carte)
 line2<-c(carte_el_douwe, douwe_el_douwe,  kenco_el_douwe, nesca_el_douwe, other_el_douwe, super_el_douwe)
  line3<-c(carte_el_kenco, douwe_el_kenco,  kenco_el_kenco, nesca_el_kenco, other_el_kenco, super_el_kenco)
  line4<-c(carte_el_nesca, douwe_el_nesca,  kenco_el_nesca, nesca_el_nesca, other_el_nesca, super_el_nesca)
 line5<-c(carte_el_other, douwe_el_other,  kenco_el_other, nesca_el_other, other_el_other, super_el_other)
  line6<-c(carte_el_super, douwe_el_super,  kenco_el_super, nesca_el_super, other_el_super, super_el_super)

# el. matrix
 el_matrix_semi_log_light<-matrix(c(line1,line2,line3,line4,line5,line6),ncol=6,byrow=TRUE)
colnames(el_matrix_semi_log_light)<-c("carte", "douwe",  "kenco", "nesca", "other", "super")  
rownames(el_matrix_semi_log_light)<-c("carte", "douwe",  "kenco", "nesca", "other", "super")
round(el_matrix_semi_log_light,3)
  
  
  

# semi log model heavy

data<-heavy
smp_size<-floor(nrow(data)*0.75)
set.seed(123)
train_ind<-sample(seq_len(nrow(data)),size=smp_size)
train<-data[train_ind, ]
test<-data[-train_ind, ]

carte <- lm(log(carte_noire_sales) ~ ., data = train)
 douwe <- lm(log(douwe_egbert_sales) ~ ., data = train)
 kenco <- lm(log(kenco_sales)~ ., data = train)
 nesca <- lm(log(nescafe_sales) ~ ., data = train)
 other <- lm(log(other_brands_sales) ~ ., data = train)
 super <- lm(log(supermarket_own_sales) ~ ., data = train)
 

  # elasticity
  
  carte_el_carte<-mean(train$carte_noire_price)*summary(carte)$coefficients[3]
 douwe_el_carte<-mean(train$douwe_egbert_price)*summary(carte)$coefficients[6]
  kenco_el_carte<-mean(train$kenco_price)*summary(carte)$coefficients[10]
 nesca_el_carte <-mean(train$nescafe_price)*summary(carte)$coefficients[14]
 other_el_carte <-mean(train$other_brands_price)*summary(carte)$coefficients[18]
 super_el_carte<-mean(train$supermarket_own_price)*summary(carte)$coefficients[22]
  
  
  carte_el_douwe<-mean(train$carte_noire_price)*summary(douwe)$coefficients[3]
 douwe_el_douwe<-mean(train$douwe_egbert_price)*summary(douwe)$coefficients[7]
  kenco_el_douwe<-mean(train$kenco_price)*summary(douwe)$coefficients[10]
 nesca_el_douwe <-mean(train$nescafe_price)*summary(douwe)$coefficients[14]
 other_el_douwe <-mean(train$other_brands_price)*summary(douwe)$coefficients[18]
 super_el_douwe<-mean(train$supermarket_own_price)*summary(douwe)$coefficients[22]
  
  
  carte_el_kenco<-mean(train$carte_noire_price)*summary(kenco)$coefficients[3]
 douwe_el_kenco<-mean(train$douwe_egbert_price)*summary(kenco)$coefficients[7]
  kenco_el_kenco<-mean(train$kenco_price)*summary(kenco)$coefficients[11]
 nesca_el_kenco <-mean(train$nescafe_price)*summary(kenco)$coefficients[14]
 other_el_kenco <-mean(train$other_brands_price)*summary(kenco)$coefficients[18]
 super_el_kenco<-mean(train$supermarket_own_price)*summary(kenco)$coefficients[22]
  
  carte_el_nesca<-mean(train$carte_noire_price)*summary(nesca)$coefficients[3]
 douwe_el_nesca<-mean(train$douwe_egbert_price)*summary(nesca)$coefficients[7]
  kenco_el_nesca<-mean(train$kenco_price)*summary(nesca)$coefficients[11]
 nesca_el_nesca <-mean(train$nescafe_price)*summary(nesca)$coefficients[15]
 other_el_nesca <-mean(train$other_brands_price)*summary(nesca)$coefficients[18]
 super_el_nesca<-mean(train$supermarket_own_price)*summary(nesca)$coefficients[22]
 
  carte_el_other<-mean(train$carte_noire_price)*summary(other)$coefficients[3]
 douwe_el_other<-mean(train$douwe_egbert_price)*summary(other)$coefficients[7]
  kenco_el_other<-mean(train$kenco_price)*summary(other)$coefficients[11]
 nesca_el_other <-mean(train$nescafe_price)*summary(other)$coefficients[15]
 other_el_other <-mean(train$other_brands_price)*summary(other)$coefficients[19]
 super_el_other<-mean(train$supermarket_own_price)*summary(other)$coefficients[22]
  
    carte_el_super<-mean(train$carte_noire_price)*summary(super)$coefficients[3]
 douwe_el_super<-mean(train$douwe_egbert_price)*summary(super)$coefficients[7]
  kenco_el_super<-mean(train$kenco_price)*summary(super)$coefficients[11]
 nesca_el_super <-mean(train$nescafe_price)*summary(super)$coefficients[15]
 other_el_super <-mean(train$other_brands_price)*summary(super)$coefficients[19]
 super_el_super<-mean(train$supermarket_own_price)*summary(super)$coefficients[23]
 
 line1<-c(carte_el_carte, douwe_el_carte,  kenco_el_carte, nesca_el_carte, other_el_carte, super_el_carte)
 line2<-c(carte_el_douwe, douwe_el_douwe,  kenco_el_douwe, nesca_el_douwe, other_el_douwe, super_el_douwe)
  line3<-c(carte_el_kenco, douwe_el_kenco,  kenco_el_kenco, nesca_el_kenco, other_el_kenco, super_el_kenco)
  line4<-c(carte_el_nesca, douwe_el_nesca,  kenco_el_nesca, nesca_el_nesca, other_el_nesca, super_el_nesca)
 line5<-c(carte_el_other, douwe_el_other,  kenco_el_other, nesca_el_other, other_el_other, super_el_other)
  line6<-c(carte_el_super, douwe_el_super,  kenco_el_super, nesca_el_super, other_el_super, super_el_super)

# el_matrix

 el_matrix_semi_log_heavy<-matrix(c(line1,line2,line3,line4,line5,line6),ncol=6,byrow=TRUE)
colnames(el_matrix_semi_log_heavy)<-c("carte", "douwe",  "kenco", "nesca", "other", "super")  
rownames(el_matrix_semi_log_heavy)<-c("carte", "douwe",  "kenco", "nesca", "other", "super")
round(el_matrix_semi_log_heavy,3)
  
  
  
  
