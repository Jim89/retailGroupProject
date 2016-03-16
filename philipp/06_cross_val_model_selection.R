# Step 0 - set up environment --------------------------------------------------
# Source the data
source("./r/analysis.R")

# Set which data set to use (e.g. heavy or light)
# REMEMBER TO CHANGE THIS IF FOR TESTING LIGHT VS. HEAVY USERS
data <- heavy

# Define function to create test and cross-validation indices on data. Sourced from:
# http://stackoverflow.com/questions/7402313/generate-sets-for-cross-validation-in-r
f_K_fold <- function(Nobs,K=5){
  rs <- runif(Nobs)
  id <- seq(Nobs)[order(rs)]
  k <- as.integer(Nobs*seq(1,K-1)/K)
  k <- matrix(c(0,rep(k,each=2),Nobs),ncol=2,byrow=TRUE)
  k[,1] <- k[,1]+1
  l <- lapply(seq.int(K),function(x,k,d) 
    list(train=d[!(seq(d) %in% seq(k[x,1],k[x,2]))],
         cv=d[seq(k[x,1],k[x,2])]),k=k,d=id)
  return(l)
  
}

# Step 1 - set up cross-validation parameters ----------------------------------
# Set k in k-fold
K <- 5

# Set the seed to get reproducible cross-validation partitions
set.seed(19891110)

# Generate train and cross-validation set partition indices
folds <- f_K_fold(nrow(data), K = K)

# Create object ot hold average MSE for the 6 models for each k-fold CV iteration
errors <- rep(NA, K)

# decide which variables to log
log_variables = vector(mode="list", length=length(heavy))
names(log_variables) = names(heavy)

for (name in names(log_variables)) {
  log_variables[name] = FALSE
}

log_variables$carte_noire_sales = TRUE
log_variables$douwe_egbert_sales = TRUE
log_variables$kenco_sales = TRUE
log_variables$nescafe_sales = TRUE
log_variables$other_brands_sales = TRUE
log_variables$supermarket_own_sales = TRUE

for (name in names(log_variables)) {
  if (log_variables[name] == TRUE){
    data[name] <- data[name] %>% log
  }
}

# Step 2 - perform k-fold validation -------------------------------------------
for (i in 1:K) {
  
  # Get train and CV-set indices from helper function output
  train_idx <- folds[[i]]$train
  cv_idx <- folds[[i]]$cv
  
  # Split the data in to train and cross-validation set using those indices
  # Drop the first column (the week number) as it is redundant in the model)
  train <- data[train_idx, -1]
  cv <- data[cv_idx, -1]
  
  # Set up overall regression for each brand using all variables
  # REMEMBER TO CHANGE THE FUNCTIONAL FORM FOR LOG-LEVEL AND LOG-LOG
  # n.b. the dot (".") syntax stands for "all other variables in the data")
  carte <- lm(carte_noire_sales ~ ., data = train)
  douwe <- lm(douwe_egbert_sales ~ ., data = train)
  kenco <- lm(kenco_sales ~ ., data = train)
  nesca <- lm(nescafe_sales ~ ., data = train)
  other <- lm(other_brands_sales ~ ., data = train)
  super <- lm(supermarket_own_sales ~ ., data = train)
  
  # Stepwise regression to find best model for each brand
  carte_fit <- step(carte, direction = "both", trace = FALSE)
  douwe_fit <- step(douwe, direction = "both", trace = FALSE) 
  kenco_fit <- step(kenco, direction = "both", trace = FALSE) 
  nesca_fit <- step(nesca, direction = "both", trace = FALSE)
  other_fit <- step(other, direction = "both", trace = FALSE)
  super_fit <- step(super, direction = "both", trace = FALSE)
  
  # Generated predicted sales with the cross-validation set for each brand
  carte_pred <- predict(carte_fit, newdata = cv)
  douwe_pred <- predict(douwe_fit, newdata = cv)
  kenco_pred <- predict(kenco_fit, newdata = cv)
  nesca_pred <- predict(nesca_fit, newdata = cv)  
  other_pred <- predict(other_fit, newdata = cv)
  super_pred <- predict(super_fit, newdata = cv)
  
  # Create a small helper function to compute Mean Squared Error
  mse <- function(actual, predicted) {
    diff <- actual - predicted
    diff_sq <- diff^2
    return(mean(diff_sq, na.rm = TRUE))
  }

  # Compute the cross-validation error (MSE) for each brand
  carte_error <- mse(cv$carte_noire_sales, carte_pred)
  douwe_error <- mse(cv$douwe_egbert_sales, douwe_pred)
  kenco_error <- mse(cv$kenco_sales, kenco_pred)
  nesca_error <- mse(cv$nescafe_sales, nesca_pred)
  other_error <- mse(cv$other_brands_sales, other_pred)
  super_error <- mse(cv$supermarket_own_sales, super_pred)
  
  # Compute average error across each of the 6 brands
  avg_error <- mean(carte_error, douwe_error, kenco_error, nesca_error,
                    other_error, super_error, na.rm = TRUE)
  
  # Pass average error back to errors object
  errors[i] <- avg_error
}

# Step 3 - Compute single average of errors from accross the CV iterations -----
overall_average_error <- mean(errors, na.rm = TRUE)

print(overall_average_error)

