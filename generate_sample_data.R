# generate_sample_data.R
# Creates synthetic dataset for 2005-2009 with structure described in the prompt.
# Training: 2005-2007 (contains missing values coded as '?')
# Public test: 2008 (no missing)
# Private test: 2009 (no missing)


set.seed(2025)
library(dplyr)


n_per_year <- 5000
make_list <- c('Toyota','Ford','Honda','BMW','Mercedes','Hyundai')
model_list <- c('M1','M2','M3','M4')
submodel_list <- c('Base','Sport','Luxury')


create_year <- function(year, n, missing=FALSE, id_start=1) {
  df <- data.frame(
    ID = seq(id_start, id_start + n -1),
    year = year,
    policy_length_months = sample(c(6,12,24), n, replace=TRUE, prob=c(0.1,0.8,0.1)),
    vehicle_make = sample(make_list, n, replace=TRUE),
    vehicle_model = sample(model_list, n, replace=TRUE),
    vehicle_submodel = sample(submodel_list, n, replace=TRUE),
    vehicle_year = sample((year-20):(year), n, replace=TRUE),
    driver_age = sample(18:80, n, replace=TRUE),
    annual_mileage = round(rnorm(n, mean=15000, sd=6000)),
    prior_accidents = rpois(n, lambda=0.2),
    policy_type = sample(c('Comprehensive','Third-Party','Third-Party Fire & Theft'), n, replace=TRUE),
    household_id = sample(100000:199999, n, replace=TRUE),
    calendar_year = year,
    stringsAsFactors = FALSE
  )
  # generate claim amount (target) with many zeros and heavy tail
  base_prob <- plogis((df$driver_age - 35)/20 + 0.5*(df$prior_accidents))
  has_claim <- rbinom(n,1, prob = pmin(base_prob, 0.9))
  amount <- ifelse(has_claim==1, rgamma(n, shape=2 + df$prior_accidents, scale=1000), 0)
  df$claim_amount <- round(amount, 2)
  
  
  # Introduce some missing values if requested (only training data)
  if(missing) {
    for(col in c('vehicle_make','vehicle_model','driver_age','annual_mileage')){
      idx <- sample(1:n, size = floor(0.05 * n))
      df[idx, col] <- '?'
    }
  }
  return(df)
}


train <- bind_rows(
  create_year(2005, n_per_year, missing=TRUE, id_start=1000001),
  create_year(2006, n_per_year, missing=TRUE, id_start=1000001 + n_per_year),
  create_year(2007, n_per_year, missing=TRUE, id_start=1000001 + 2*n_per_year)
)


test2008 <- create_year(2008, n_per_year, missing=FALSE, id_start=2000001)
test2009 <- create_year(2009, n_per_year, missing=FALSE, id_start=3000001)


# Save CSVs
write.csv(train, 'train_2005_2007.csv', row.names = FALSE, na = '')
write.csv(test2008, 'test_2008.csv', row.names = FALSE, na = '')
write.csv(test2009, 'test_2009.csv', row.names = FALSE, na = '')


cat('Sample datasets created: train_2005_2007.csv, test_2008.csv, test_2009.csv\n')
