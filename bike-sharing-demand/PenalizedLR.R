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


preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R
preg_wf <- workflow() %>%
add_recipe(bike_recipe) %>%
add_model(preg_model)

grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = 4) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(trainData, v = 10, repeats=1)

## Run the CV
CV_results <- preg_wf %>%
tune_grid(resamples=folds,
          grid=grid_of_tuning_params,
          metrics=metric_set(rmse, mae)) #Or leave metrics NULL

## Plot Results (example)
collect_metrics(CV_results) %>% # Gathers metrics into DF
  filter(.metric=="rmse") %>%
ggplot(data=., aes(x=penalty, y=mean, color=factor(mixture))) +
geom_line()

## Find Best Tuning Parameters
bestTune <- CV_results %>%
select_best(metric="rmse")

## Finalize the Workflow & fit it1
final_wf <-
preg_wf %>%
finalize_workflow(bestTune) %>%
fit(data=trainData)

## Predict
lin_preds <- predict(final_wf, new_data = testData)
lin_preds$.pred <- exp(lin_preds$.pred)
  
#fit(data=trainData)
#lin_preds <- predict(preg_wf, new_data=testData)
#lin_preds$.pred <- exp(lin_preds$.pred)

#Kaggle submission fomratting
kaggle_submission <- lin_preds |>
  bind_cols(testData) |> #Bind predictions with test data
  select(datetime, .pred) |> #Just keep datetime and prediction variables
  rename(count=.pred) |> #rename pred to count (for submission to Kaggle)
  mutate(count=pmax(0, count)) |> #pointwise max of (0, prediction)
  mutate(datetime=as.character(format(datetime))) #needed for right format to Kaggle

## Write out the file
vroom_write(x=kaggle_submission, file="./LinearPreds.csv", delim=",")