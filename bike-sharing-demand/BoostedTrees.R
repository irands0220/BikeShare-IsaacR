library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)
library(bonsai)
library(lightgbm)

# Load data
trainData <- vroom("BikeShare-IsaacR/bike-sharing-demand/train.csv") |>
  select(-c(casual, registered)) |>
  mutate(count = log(count))
testData <- vroom("BikeShare-IsaacR/bike-sharing-demand/test.csv")

# Define random forest model with tuning
boost_model <- boost_tree(tree_depth=tune(),
                          trees=tune(),
                          learn_rate=tune()) %>%
set_engine("lightgbm") %>% #or "xgboost" but lightgbm is faster
  set_mode("regression")

# Define recipe (simplified for RF)
bike_recipe <- recipe(count ~ ., data = trainData) %>%
  step_mutate(
    weather = ifelse(weather == 4, 3, weather),
    weather = factor(weather),
    season  = factor(season)
  ) %>%
  step_mutate(tempHum = temp * humidity) %>%
  step_time(datetime, features = "hour") %>%
  step_rm(datetime) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) # remove zero variance predictors

# Create workflow
boost_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(boost_model)

# Extract and finalize parameter set
boost_params <- extract_parameter_set_dials(boost_wf)
boost_params <- finalize(boost_params, trainData)

# Build tuning grid
my_grid <- grid_regular(boost_params, levels = 5)

# 10-fold CV
folds <- vfold_cv(trainData, v = 10)

# Tune model
CV_results <- boost_wf %>%
  tune_grid(
    resamples = folds,
    grid = my_grid,
    metrics = metric_set(rmse, mae)
  )

# Select best parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

# Finalize workflow with best parameters and fit
final_wf <- boost_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = trainData)

# Predict on test set
lin_preds <- predict(final_wf, new_data = testData)
lin_preds$.pred <- exp(lin_preds$.pred)  # backtransform

# Format Kaggle submission
kaggle_submission <- lin_preds |>
  bind_cols(testData) |>
  select(datetime, .pred) |>
  rename(count = .pred) |>
  mutate(count = pmax(0, count)) |>
  mutate(datetime = as.character(format(datetime)))

# Write CSV
vroom_write(x = kaggle_submission, file = "./RandomForestPreds.csv", delim = ",")
