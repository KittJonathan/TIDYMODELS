# MACHINE LEARNING WITH TIDYMODELS
# POSIT::CONF 2024 WORKSHOPS
# https://workshops.tidymodels.org/
# INTRODUCTION TO TIDYMODELS
# 3 - WHAT MAKES A MODEL?
# https://workshops.tidymodels.org/slides/intro-03-what-makes-a-model.html#/title-slide

# 📦 LOAD PACKAGES --------------------------------------------------------

library(tidymodels)
library(forested)
library(rpart.plot)

# 🔎 EXPLORE THE DATASET --------------------------------------------------

?forested
glimpse(forested)
forested

# ➗ DATA SPLITTING AND SPENDING -------------------------------------------

# Initial split
set.seed(123)
forested_split <- initial_split(forested, prop = 0.8)

# Accessing the data
forested_train <- training(forested_split)
forested_test <- testing(forested_split)

nrow(forested_train)
nrow(forested_test)

# 🔎 EXPLORE THE TRAINING DATA --------------------------------------------

theme_set(theme_bw())

forested_train |> 
  ggplot(aes(x = forested)) +
  geom_bar()

forested_train |> 
  ggplot(aes(x = forested,
             fill = tree_no_tree)) +
  geom_bar()

forested_train |> 
  ggplot(aes(x = precip_annual, fill = forested, group = forested)) +
  geom_histogram(position = "identity", alpha = .7)

forested_train |> 
  ggplot(aes(x = precip_annual, fill = forested, group = forested)) +
  geom_histogram(position = "fill")

forested_train |> 
  ggplot(aes(x = lon, y = lat, col = forested)) +
  geom_point()

# ➕ SPECIFY A MODEL -------------------------------------------------------

# 1. Choose a model

logistic_reg()

# 2. Specify an engine

logistic_reg() |> 
  set_engine("glmnet")

logistic_reg() |> 
  set_engine("stan")

# 3. Set the mode

decision_tree()

decision_tree() |> 
  set_mode("classification")

tree_spec <- decision_tree() |> 
  set_mode("classification")

tree_spec

tree_spec <- decision_tree() %>% 
  set_mode("classification")

tree_spec

#️ ➡️ A MODEL WORKFLOW ---------------------------------------------------

tree_spec <- 
  decision_tree() |> 
  set_mode("classification")

tree_spec |> 
  fit(forested ~ ., data = forested_train)

workflow() |> 
  add_formula(forested ~ .) |> 
  add_model(tree_spec) |> 
  fit(data = forested_train)

workflow(forested ~ ., tree_spec) |>
  fit(data = forested_train)
  

tree_spec <-
  decision_tree() |> 
  set_mode("classification")

tree_wflow <- workflow() |> 
  add_formula(forested ~ .) |> 
  add_model(tree_spec)

tree_fit <-
  tree_wflow |> 
  fit(data = forested_train) 

predict(tree_fit, new_data = forested_test)

augment(tree_fit, new_data = forested_test)

tree_fit |> 
  extract_fit_engine() |> 
  rpart.plot(roundint = FALSE)
