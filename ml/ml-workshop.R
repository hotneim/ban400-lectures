## ------
## ------    BAN400
## ------    WORKSHOP:      E-MAIL SPAM FILTER
## ------    DATA SOURCE:   https://archive.ics.uci.edu/ml/datasets/spambase
## ------

# Load packages -----
library(readr)
library(dplyr)
library(tidymodels)
library(rpart)           # For decision trees
library(rpart.plot)      # Separate package for plotting trees
library(xgboost)         # For xgboost

# Read data ------
names <- 
  read_csv("spambase/spambase.names", 
         skip = 32,
         col_names = FALSE) %>% 
  separate(X1,
           into = c("name", "drop"),
           sep = ":") %>% 
  select(-drop) %>% 
  bind_rows(tibble(name = "spam")) %>% 
  pull
  

spam <- 
  read_csv("spambase/spambase.data", col_names = names) %>% 
  mutate(spam = as.factor(spam))

# What is the distribution of spam e-mail in the data set?
spam %>% 
  group_by(spam) %>% 
  summarize(n_emails = n()) %>% 
  mutate(share = n_emails/sum(n_emails))

# Split the data into training and test data, and divide the training data into
# folds for cross-validaton.
set.seed(1)
spam_split <- initial_split(spam, strata = spam)
spam_train <- training(spam_split)
spam_test  <- testing (spam_split)

spam_folds <- vfold_cv(spam_train, strata = spam, v = 3)  # v = 5 or 10 is more common

# Specify the recipe, that is common for all models
spam_recipe <- 
  recipe(spam ~ ., data = spam) 

## DECISION TREE -------------

# Specify the decistion tree
tree_mod <- 
  decision_tree(
    tree_depth = tune(),
    min_n = tune()) %>%
  set_mode("classification") %>% 
  set_engine("rpart") 

# Set up the workflow
tree_workflow <- 
  workflow() %>% 
  add_model(tree_mod) %>% 
  add_recipe(spam_recipe)

# Make a search grid for the k-parameter
tree_grid <- 
  grid_latin_hypercube(
    tree_depth(),
    min_n(),
    size = 10
)

# Calculate the cross-validated AUC for all the k's in the grid
tree_tune_result <- 
  tune_grid(
    tree_workflow,
    resamples = spam_folds,
    grid = tree_grid,
    control = control_grid(save_pred = TRUE)
  )

# Which parameter combination is the best?
tree_tune_result %>%
  select_best(metric = "roc_auc") 

# Put the best parameters in the workflow
tree_tuned <- 
  finalize_workflow(
    tree_workflow,
    parameters = tree_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_tree <- 
  tree_tuned %>% 
  fit(data = spam_train)

# Plot the model
rpart.plot(fitted_tree$fit$fit$fit)

# Predict the train and test data
predictions_tree_test <- 
  fitted_tree %>% 
  predict(new_data = spam_test,
          type = "prob") %>% 
  mutate(truth = spam_test$spam) 

predictions_tree_train <- 
  fitted_tree %>% 
  predict(new_data = spam_train,
          type = "prob") %>% 
  mutate(truth = spam_train$spam) 


# Calculate the AUC
auc_tree <-
  predictions_tree_test %>% 
  roc_auc(truth, .pred_0) %>% 
  mutate(where = "test") %>% 
  bind_rows(predictions_tree_train %>% 
              roc_auc(truth, .pred_0) %>% 
              mutate(where = "train")) %>% 
  mutate(model = "decision_tree")

# XGBOOST -----------
# Taken from: https://juliasilge.com/blog/xgboost-tune-volleyball/


# Specify the decistion tree
xgb_mod <- 
  boost_tree(
    trees = 1000, 
    tree_depth = tune(), min_n = tune(), 
    loss_reduction = tune(),                     ## first three: model complexity
    sample_size = tune(), mtry = tune(),         ## randomness
    learn_rate = tune()                          ## step size
  ) %>% 
  set_engine("xgboost") %>% 
  set_mode("classification")

# Set up the workflow
xgb_workflow <- 
  workflow() %>% 
  add_model(xgb_mod) %>% 
  add_recipe(spam_recipe)

# Make a search grid for the k-parameter
xgb_grid <- grid_latin_hypercube(
  tree_depth(),
  min_n(),
  loss_reduction(),
  sample_size = sample_prop(),
  finalize(mtry(), spam_train),
  learn_rate(),
  size = 30
)

# Calculate the cross-validated AUC for all the parameter configurations in the
# grid Takes some time, so we save and load.
# xgb_tune_result <- 
#   tune_grid(
#     xgb_workflow,
#     resamples = spam_folds,
#     grid = xgb_grid,
#     control = control_grid(save_pred = TRUE)
#   )
# save(xgb_tune_result, file = "xgb_tune_result.Rdata")

load("xgb_tune_result.Rdata")

# Which parameter combination is the best?
xgb_tune_result %>%
  select_best(metric = "roc_auc") 

# Put the best parameters in the workflow
xgb_tuned <- 
  finalize_workflow(
    xgb_workflow,
    parameters = xgb_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_xgb <- 
  xgb_tuned %>% 
  fit(data = spam_train)

# Predict the train and test data
predictions_xgb_test <- 
  fitted_xgb %>% 
  predict(new_data = spam_test,
          type = "prob") %>% 
  mutate(truth = spam_test$spam) 

predictions_xgb_train <- 
  fitted_xgb %>% 
  predict(new_data = spam_train,
          type = "prob") %>% 
  mutate(truth = spam_train$spam) 


# Calculate the AUC
auc_xgb <- 
  predictions_xgb_test %>% 
  roc_auc(truth, .pred_0) %>% 
  mutate(where = "test") %>% 
  bind_rows(predictions_xgb_train %>% 
              roc_auc(truth, .pred_0) %>% 
              mutate(where = "train")) %>% 
  mutate(model = "xgb") 

bind_rows(auc_tree, auc_xgb)


