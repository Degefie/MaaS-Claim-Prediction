# Model-as-a-Service for Insurance Claim Prediction (R)


## Overview
This repository contains R scripts to:
1. Generate synthetic sample data (`generate_sample_data.R`).
2. Train an XGBoost model (`train_model.R`).
3. Produce batch predictions (`predict_batch.R`).
4. Expose the model as a REST API via `plumber` (`api.R`).


## Quick start (local)
1. Create sample data:
```bash
Rscript generate_sample_data.R
