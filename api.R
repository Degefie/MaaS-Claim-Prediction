# api_swagger_json.R
library(plumber)
library(data.table)
library(xgboost)
library(jsonlite)
source('utils.R')

# Load model and columns
model <<- xgb.load('xgb_claim.model')
model_cols <<- readRDS('model_cols.rds')

#* @apiTitle Claim Prediction API
#* @parser json
#* @post /predict
#* @param body:list A JSON array of vehicle records with household_id and other features
#* @example body [{"household_id":123456,"vehicle_make":"Toyota","vehicle_model":"Corolla","vehicle_submodel":"Base","vehicle_year":2007,"policy_type":"Standard","driver_age":35,"annual_mileage":12000,"prior_accidents":0,"calendar_year":2009}]
function(body, res) {
  
  # Check if body is NULL
  if(is.null(body)) {
    res$status <- 400
    return(list(error="Missing JSON body. Ensure Content-Type: application/json"))
  }
  
  # Convert to data.frame if needed
  if(!is.data.frame(body)) body <- as.data.frame(body, stringsAsFactors=FALSE)
  df <- data.table(body)
  
  # Required fields check
  required_cols <- c("household_id","vehicle_make","vehicle_model","vehicle_submodel",
                     "vehicle_year","policy_type","driver_age","annual_mileage",
                     "prior_accidents","calendar_year")
  missing <- setdiff(required_cols, names(df))
  if(length(missing) > 0) {
    res$status <- 400
    return(list(error=paste("Missing required fields:", paste(missing, collapse=", "))))
  }
  
  # Generate ID if missing
  if(!"ID" %in% names(df)) df$ID <- seq_len(nrow(df))
  
  # Preprocess data
  df <- clean_data(df, is_train=FALSE)
  
  # Model matrix
  mm <- model.matrix(~ . -1 -calendar_year -household_id, data=as.data.frame(df))
  
  # Fill missing columns
  missing_cols <- setdiff(model_cols, colnames(mm))
  if(length(missing_cols) > 0) {
    addmat <- matrix(0, nrow=nrow(mm), ncol=length(missing_cols))
    colnames(addmat) <- missing_cols
    mm <- cbind(mm, addmat)
  }
  
  # Order columns
  mm <- mm[, model_cols, drop=FALSE]
  
  # Predict
  preds <- predict(model, newdata=mm)
  preds[preds < 0] <- 0
  
  # Return predictions
  if(nrow(df) == 1) {
    return(list(prediction = round(as.numeric(preds[1]),2)))
  } else {
    return(list(prediction = round(as.numeric(preds),2)))
  }
}
