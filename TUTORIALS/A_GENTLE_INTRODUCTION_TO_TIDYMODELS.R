# A GENTLE INTRODUCTION TO TIDYMODELS
# https://rviews.rstudio.com/2019/06/19/a-gentle-intro-to-tidymodels/
# 2024-08-20

# PACKAGES ----------------------------------------------------------------

library(tidymodels)

# PRE-PROCESS -------------------------------------------------------------

iris_split <- initial_split(iris, prop = 0.6)
iris_split

iris_split |> 
  training() |> 
  glimpse()

iris_recipe <- training(iris_split) |> 
  recipe(Species ~ .) |> 
  step_corr(all_predictors()) |> 
  step_center(all_predictors(), -all_outcomes()) |> 
  step_scale(all_predictors(), -all_outcomes()) |> 
  prep()

iris_recipe

iris_testing <- iris_recipe |> 
  bake(testing(iris_split))

glimpse(iris_testing)

iris_training <- juice(iris_recipe)
glimpse(iris_training)

# MODEL TRAINING ----------------------------------------------------------

iris_ranger <- rand_forest(trees = 100, mode = "classification") |> 
  set_engine("ranger") |> 
  fit(Species ~ ., data = iris_training)

iris_rf <- rand_forest(trees = 100, mode = "classification") |> 
  set_engine("randomForest") |> 
  fit(Species ~ ., data = iris_training)

predict(iris_ranger, iris_testing)

# MODEL VALIDATION --------------------------------------------------------

iris_ranger |> 
  predict(iris_testing) |> 
  bind_cols(iris_testing) |> 
  glimpse()

iris_ranger |> 
  predict(iris_testing) |> 
  bind_cols(iris_testing) |> 
  metrics(truth = Species, estimate = .pred_class)

iris_rf |> 
  predict(iris_testing) |> 
  bind_cols(iris_testing) |> 
  metrics(truth = Species, estimate = .pred_class)

iris_ranger |> 
  predict(iris_testing, type = "prob") |> 
  glimpse()

iris_probs <- iris_ranger |> 
  predict(iris_testing, type = "prob") |> 
  bind_cols(iris_testing)

glimpse(iris_probs)

iris_probs |> 
  gain_curve(Species, .pred_setosa:.pred_virginica) |> 
  glimpse()

iris_probs |> 
  gain_curve(Species, .pred_setosa:.pred_virginica) |> 
  autoplot()

iris_probs |> 
  roc_curve(Species, .pred_setosa:.pred_virginica) |> 
  autoplot()

predict(iris_ranger, iris_testing, type = "prob") |> 
  bind_cols(predict(iris_ranger, iris_testing)) |> 
  bind_cols(select(iris_testing, Species)) |> 
  glimpse()

predict(iris_ranger, iris_testing, type = "prob") |> 
  bind_cols(predict(iris_ranger, iris_testing)) |> 
  bind_cols(select(iris_testing, Species)) |> 
  metrics(Species, .pred_setosa:.pred_virginica, estimate = .pred_class)
