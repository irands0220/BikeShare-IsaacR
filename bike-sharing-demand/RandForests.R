library(tidyverse)
library(tidymodels)
library(vroom)
library(patchwork)
library(DataExplorer)
library(GGally)

trainData <- vroom("BikeShare-IsaacR/bike-sharing-demand/train.csv")|>
  select(-c(casual,registered)) |>
  mutate(count = log(count))
testData <- vroom("BikeShare-IsaacR/bike-sharing-demand/test.csv")

forest_mod <- rand_forest(mtry = tune(),
                          min_n=tune(),
                          trees=500) %>% #Type of model
  set_engine("ranger") %>% # What R function to use
  set_mode("regression")

## Create a workflow with model & recipe
bike_recipe <- recipe(count ~ ., data = trainData) %>% 
  step_mutate(
    weather = ifelse(weather == 4, 3, weather),
    weather = factor(weather),
    season  = factor(season)
  ) %>%
  step_mutate(tempHum=temp*humidity) %>%
  step_time(datetime, features = "hour") %>%
  step_rm(datetime) %>%   # remove raw datetime
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_poly(temp, degree=4) %>% #Create polynomial expansion of var
  step_corr(all_numeric_predictors(), threshold = 0.8)%>%   # last step ends cleanly
  step_normalize(all_numeric_predictors()) # Make mean 0, sd=1

prepped_recipe <- prep(bike_recipe)
baked_data <- bake(prepped_recipe, new_data = trainData)

forest_wf <- workflow() %>%
  add_recipe(bike_recipe) %>%
  add_model(forest_mod)

## Set up grid of tuning values
forest_params <- extract_parameter_set_dials(forest_wf)
forest_params <- finalize(forest_params, trainData)
grid_of_tuning_params <- grid_regular(forest_params, levels = 4)

## Set up K-fold CV
folds <- vfold_cv(trainData, v = 10)

# Tune the tree
CV_results <- forest_wf %>%
  tune_grid(
    resamples = folds,
    grid = grid_of_tuning_params,
    metrics = metric_set(rmse, mae)
  )

# Select best parameters
bestTune <- CV_results %>%
  select_best(metric = "rmse")

# Finalize and fit
final_wf <- forest_wf %>%
  finalize_workflow(bestTune) %>%
  fit(data = trainData)

# Predict on test
lin_preds <- predict(final_wf, new_data = testData)
lin_preds$.pred <- exp(lin_preds$.pred)

#Kaggle submission fomratting
kaggle_submission <- lin_preds |>
  bind_cols(testData) |> #Bind predictions with test data
  select(datetime, .pred) |> #Just keep datetime and prediction variables
  rename(count=.pred) |> #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) |> #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")