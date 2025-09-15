# train_model.R
# Train an XGBoost model and save it to disk (xgboost + caret used)

setwd("~/MaaS-Claim-Prediction")

library(data.table)
library(xgboost)
library(Matrix)
library(caret)
source('utils.R')


# Read
train <- fread('train_2005_2007.csv', na.strings = c('', 'NA'))
train <- clean_data(train, is_train=TRUE)


# Create target and features
y <- train$claim_amount
train$claim_amount <- NULL


# One-hot encode categorical variables using model.matrix
cat_cols <- c('vehicle_make','vehicle_model','vehicle_submodel','policy_type')
num_cols <- setdiff(names(train), c('ID','calendar_year','household_id') )
num_cols <- setdiff(num_cols, cat_cols)


# For xgboost, build sparse matrix
mm <- model.matrix(~ . -1 - ID -calendar_year -household_id, data=as.data.frame(train))
dmat <- xgb.DMatrix(data = mm, label = y)


# Simple cross-validation for hyperparams
params <- list(
  objective = 'reg:squarederror',
  eval_metric = 'rmse',
  eta = 0.05,
  max_depth = 6,
  subsample = 0.8,
  colsample_bytree = 0.7
)
cv <- xgb.cv(params = params, data = dmat, nrounds = 1000, nfold = 5, early_stopping_rounds = 20, verbose = 1)
best_nrounds <- cv$best_iteration


model <- xgb.train(params = params, data = dmat, nrounds = best_nrounds)


# Save model and column names
xgb.save(model, 'xgb_claim.model')
cols <- colnames(mm)
saveRDS(cols, file='model_cols.rds')


cat('Model trained and saved: xgb_claim.model and model_cols.rds\n')

