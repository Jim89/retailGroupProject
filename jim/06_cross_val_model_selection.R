# Function to create test and train indices
# Sourced from:
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

# Set k in k-fold
K <- 5

# Generate train and cross-validation set partition indices
folds <- f_K_fold(nrow(heavy), K = K)

# Create object ot hold average MSE for the 6 models for each k-fold CV iteration
errors <- rep(NA, K)

# Perform k-fold CV
for (i in 1:K) {
  
  # grab the pre-computed indices
  train_idx <- folds[[i]]$train
  cv_idx <- folds[[i]]$cv
  
  # Split in to train and cross-validation set
  train <- heavy[train_idx, -1]
  cv <- heavy[cv_idx, -1]
  
  # Regress for each brand's sales using all variables
  carte <- lm(carte_noire_sales ~ ., data = train)
  douwe <- lm(douwe_egbert_sales ~ ., data = train)
  kenco <- lm(kenco_sales ~ ., data = train)
  nesca <- lm(nescafe_sales ~ ., data = train)
  other <- lm(other_brands_sales ~ ., data = train)
  super <- lm(supermarket_own_sales ~ ., data = train)
  
  # Stepwise regression to find best model
  carte_fit <- step(carte, direction = "both", trace = FALSE)
  douwe_fit <- step(douwe, direction = "both", trace = FALSE) 
  kenco_fit <- step(kenco, direction = "both", trace = FALSE) 
  nesca_fit <- step(nesca, direction = "both", trace = FALSE)
  other_fit <- step(other, direction = "both", trace = FALSE)
  super_fit <- step(super, direction = "both", trace = FALSE)
  
  # Predict on cross-val set
  carte_pred <- predict(carte_fit, newdata = cv)
  douwe_pred <- predict(douwe_fit, newdata = cv)
  kenco_pred <- predict(kenco_fit, newdata = cv)
  nesca_pred <- predict(nesca_fit, newdata = cv)  
  other_pred <- predict(other_fit, newdata = cv)
  super_pred <- predict(super_fit, newdata = cv)
  
  # Compute errors (MSE)
  mse <- function(actual, predicted) {
    diff <- actual - predicted
    diff_sq <- diff^2
    mean(diff_sq, na.rm = TRUE)
  }
  
  carte_error <- mse(cv$carte_noire_sales, carte_pred)
  douwe_error <- mse(cv$douwe_egbert_sales, douwe_pred)
  kenco_error <- mse(cv$kenco_sales, kenco_pred)
  nesca_error <- mse(cv$nescafe_sales, nesca_pred)
  other_error <- mse(cv$other_brands_sales, other_pred)
  super_error <- mse(cv$supermarket_own_sales, super_pred)
  
  # Compute average error
  avg_error <- mean(carte_error, douwe_error, kenco_error, nesca_error,
                    other_error, super_error, na.rm = TRUE)
  
  # Pass average error back to errors object
  errors[i] <- avg_error
}

# Compute average of average error from accross the CV iterations
overall_average_error <- mean(errors, na.rm = TRUE)

