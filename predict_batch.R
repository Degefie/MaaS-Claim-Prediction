# predict_batch.R
# Load model, preprocess new data, produce predictions CSV in required format
library(data.table)
library(xgboost)
library(Matrix)
source('utils.R')


args <- commandArgs(trailingOnly = TRUE)


# Friendlier: allow defaults if no args supplied
if(length(args) < 2) {
  message('No arguments supplied, using defaults: test_2008.csv + test_2009.csv -> predictions.csv')
  if(file.exists('test_2008.csv') && file.exists('test_2009.csv')) {
    d1 <- fread('test_2008.csv')
    d2 <- fread('test_2009.csv')
    new <- rbind(d1, d2)
    input <- 'combined_default'
    output <- 'predictions.csv'
  } else {
    stop('Usage: Rscript predict_batch.R <input.csv> <output.csv>')
  }
} else {
  input <- args[1]
  output <- args[2]
  new <- fread(input, na.strings = c('', 'NA'))
}


cols <- readRDS('model_cols.rds')
model <- xgb.load('xgb_claim.model')


new <- clean_data(new, is_train=FALSE)
ids <- new$ID
new$ID <- NULL


mm_new <- model.matrix(~ . -1 - calendar_year -household_id, data=as.data.frame(new))
# Align columns with training
missing_cols <- setdiff(cols, colnames(mm_new))
if(length(missing_cols)>0) {
  addmat <- matrix(0, nrow=nrow(mm_new), ncol=length(missing_cols))
  colnames(addmat) <- missing_cols
  mm_new <- cbind(mm_new, addmat)
}
mm_new <- mm_new[, cols]


pred <- predict(model, newdata = mm_new)
# Ensure non-negative
pred[pred < 0] <- 0


out <- data.frame(ID = ids, Prediction = round(pred, 2))
write.csv(out, output, row.names = FALSE, quote = FALSE)
cat('Prediction file written to', output, '\n')
