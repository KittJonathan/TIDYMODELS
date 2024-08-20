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


