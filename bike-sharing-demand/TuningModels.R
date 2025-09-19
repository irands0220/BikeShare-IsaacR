library(tidymodels)

## Penalized regression model
preg_model <- linear_reg(penalty=tune(),
                         mixture=tune()) %>% #Set model and tuning
  set_engine("glmnet") # Function to fit in R
7
## Set Workflow
preg_wf <- workflow() %>%
add_recipe(myRecipe) %>%
add_model(preg_model)

## Grid of values to tune over
grid_of_tuning_params <- grid_regular(penalty(),
                                      mixture(),
                                      levels = L) ## L^2 total tuning possibilities

## Split data for CV
folds <- vfold_cv(trainData, v = K, repeats=1)