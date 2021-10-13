## ------
## ------    BAN400
## ------    WORKSHOP:      SOLUTION TO THE SPAM FILTER EXERCISE
## ------                   (Assumes that the workshop script has been run)
## ------    DATA SOURCE:   https://archive.ics.uci.edu/ml/datasets/spambase
## ------


library(xgboost)         # For xgboost


# Specify the xgboost model
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
# grid Takes some time, so we save and load (the file is in the github-repo).
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

