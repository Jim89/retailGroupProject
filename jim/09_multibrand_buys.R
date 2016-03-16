trans_id <- coffee_clean %>% 
            select(relweek, day, house) %>% 
            distinct() %>% 
            mutate(transaction_id = row_number())

coffee_clean <- coffee_clean %>% left_join(trans_id)


brands_per_id <- coffee_clean %>% 
                  select(transaction_id, brand_clean) %>% 
                  distinct() %>% 
                  mutate(quant = 1,
                         transaction_id = as.character(transaction_id))

                  group_by(transaction_id) %>% 
                  summarise(brands = paste(brand_clean, collapse = ", ")) %>% 
                  mutate(quant = 1)

colocs <- brands_per_id %>% 
          group_by(brands) %>% 
          tally() %>% 
          filter(grepl(",", brands) == TRUE)

colocs_wide <- colocs %>% 
                separate(brands, 
                         into = c("brand1", "brand2", "brand3", "brand4"), ",")

colocs_mat <- matrix(nrow = 6, ncol = 6)
colnames(colocs_mat) <- colnames(heavy_elasticities_clean)
rownames(colocs_mat) <- rownames(heavy_elasticities_clean)


library(reshape2)  
dat2 <- melt(brands_per_id)
w <- dcast(dat2, brand_clean ~ transaction_id)
x <- as.matrix(w[,-1])
x[is.na(x)] <- 0
x <- apply(x, 2,  function(x) as.numeric(x > 0))  #recode as 0/1
v <- x %*% t(x)                                   #the magic matrix 
diag(v) <- 0                                      #repalce diagonal
dimnames(v) <- list(w[, 1], w[,1])                #name the dimensions
v