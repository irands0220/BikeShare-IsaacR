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
  step_corr(all_numeric_predictors(), threshold = 0.8)   # last step ends cleanly

prepped_recipe <- prep(bike_recipe)
baked_data <- bake(prepped_recipe, new_data = trainData)

# Define a Model
lin_model <- linear_reg() %>%
set_engine("lm") %>%
set_mode("regression")

## Combine into a Workflow and fit
bike_workflow <- workflow() %>%
add_recipe(bike_recipe) %>%
add_model(lin_model) %>%
fit(data=trainData)

## Run all the steps on test data
lin_preds <- predict(bike_workflow, new_data = testData)
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