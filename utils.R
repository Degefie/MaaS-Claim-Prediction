# utils.R
library(dplyr)


# Replace '?' with NA and do basic preprocessing
clean_data <- function(df, is_train=TRUE) {
  df[df == '?'] <- NA
  # Convert types
  df$vehicle_make <- as.factor(df$vehicle_make)
  df$vehicle_model <- as.factor(df$vehicle_model)
  df$vehicle_submodel <- as.factor(df$vehicle_submodel)
  df$policy_type <- as.factor(df$policy_type)
  df$prior_accidents <- as.numeric(df$prior_accidents)
  df$driver_age <- as.numeric(df$driver_age)
  df$annual_mileage <- as.numeric(df$annual_mileage)
  
  
  # Simple imputations for numeric columns
  num_cols <- c('driver_age','annual_mileage')
  for (c in num_cols) {
    if(c %in% names(df)) df[[c]][is.na(df[[c]])] <- median(df[[c]], na.rm=TRUE)
  }
  
  
  # Fill factor NA with 'Missing'
  fac_cols <- c('vehicle_make','vehicle_model','vehicle_submodel','policy_type')
  for (c in fac_cols) {
    if(c %in% names(df)) {
      df[[c]] <- addNA(df[[c]])
      levels(df[[c]])[is.na(levels(df[[c]]))] <- 'Missing'
    }
  }
  return(df)
}


# Normalized Gini (for ranking)
NormalizedGini <- function(actual, pred) {
  # from typical Kaggle implementations
  df <- data.frame(actual=actual, pred=pred)
  df <- df[order(-df$pred, df$actual), ]
  df$lorentz <- cumsum(df$actual) / sum(df$actual)
  df$random <- seq_along(df$actual) / length(df$actual)
  Gini <- sum(df$lorentz - df$random)
  # perfect Gini
  df2 <- df[order(-df$actual), ]
  df2$lorentz <- cumsum(df2$actual) / sum(df2$actual)
  df2$random <- seq_along(df2$actual) / length(df2$actual)
  GiniPerfect <- sum(df2$lorentz - df2$random)
  return(Gini / GiniPerfect)
}
