# Step 0 - load packages -------------------------------------------------------

# Step 1 - Create function that creates plot -----------------------------------
var_imp <- function(rforest) {
rownames(rforest$importance) <- rownames(rforest$importance) %>% 
                            gsub("_", " ", .) %>% 
                            gsub("avg", "Average", .) %>% 
                            gsub("prop", "proportion", .) %>% 
                            gsub("promo", "promotion", .) %>% 
                            toproper()
colnames(rforest$importance)[3] <- "Importance"
varImpPlot(rforest, type = 1, main = "", 
           cex = 1, color = "black")
}



