
## ------
## ------    BAN400
## ------    LECTURE:       MACHINE LEARNING
## ------    DATA SOURCE:   https://www.kaggle.com/blastchar/telco-customer-churn
## ------

library(dplyr)
library(ggplot2)
library(here)
library(tidymodels)      # <- This is new. Tidyverse take on modeling

no_color <- "#00000020"
yes_color <- "#FF000070"
pred_color <- c("#FFFFFF00", "#AA3333DD")
cutoff <- .5
grid_resolution <- 50

# Read the data
telco <- 
  here("telco-customer-churn", "WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
  readr::read_csv() %>% 
  select(Churn, MonthlyCharges, tenure) %>% 
  mutate(Churn = as.factor(Churn))

# Make a basic scatterplot 
scatter <- 
  telco %>% 
  ggplot(aes(x = MonthlyCharges, y = tenure, colour = Churn)) +
  geom_point() +
  xlab("Monthly charges ($)") +
  ylab("Tenure (months)") +
  scale_color_manual(values=c(no_color, yes_color)) +
  theme_classic()

scatter

# Fit a logistic regression to the data using tidymodels
fitted_logistic <-
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification") %>% 
  fit(Churn ~ ., data = telco)

# Look at the results
tidy(fitted_logistic)

# Draw the decision boundary for logistic regression:
newdata <- 
  expand.grid(MonthlyCharges = seq(from = 20, 
                                   to = 120, 
                                   length.out = grid_resolution),
            tenure = seq(from = 0, 
                         to = 80, 
                         length.out = grid_resolution))

predictions <- 
  newdata %>% 
  bind_cols(pred_prob_logistic = predict(fitted_logistic, 
                                         new_data = newdata, 
                                         type = "prob")$.pred_Yes)

scatter +
  geom_contour_filled(aes(x = MonthlyCharges, 
                          y = tenure, 
                          z = pred_prob_logistic),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = pred_color, 
                    name="Predicted probability", 
                    drop = FALSE) +
  geom_contour(aes(x = MonthlyCharges, 
                   y = tenure, 
                   z = pred_prob_logistic),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1.5)

# Do a prediction using k-nearest-neighbours (kNN)
# Requires package: "kknn"
fitted_knn <-
  nearest_neighbor(neighbors = 50) %>% 
  set_engine("kknn") %>% 
  set_mode("classification") %>% 
  fit(Churn ~ ., data = telco)

predictions <- 
  predictions %>% 
  mutate(pred_prob_knn = predict(fitted_knn, 
                                 new_data = newdata, 
                                 type = "prob")$.pred_Yes)

scatter +
  geom_contour_filled(aes(x = MonthlyCharges, 
                          y = tenure, 
                          z = pred_prob_knn),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = pred_color, 
                    name="Predicted probability", 
                    drop = FALSE) +
  geom_contour(aes(x = MonthlyCharges, 
                   y = tenure, 
                   z = pred_prob_knn),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1.5)

# Tune the kNN using cross-validation -----

# Split the data into training and testing set
set.seed(1)
telco_split <- initial_split(telco, strata = Churn)
telco_train <- training(telco_split)
telco_test  <- testing (telco_split)

telco_folds <- vfold_cv(telco_train, strata = Churn)

# Specify the model 
knn_mod <- 
  nearest_neighbor(neighbors = tune()) %>% 
  set_mode("classification") %>% 
  set_engine("kknn") 
  
# Specify the recipe
knn_recipe <- 
  recipe(Churn ~ ., data = telco)

# Set up the workflow
knn_workflow <- 
  workflow() %>% 
  add_model(knn_mod) %>% 
  add_recipe(knn_recipe)

# Make a search grid for the k-parameter
knn_grid <- grid_latin_hypercube(
  neighbors(c(3, round(nrow(telco)/5))),
  size = 15
)

# Calculate the cross-validated AUC for all the k's in the grid
knn_tune_result <- 
  tune_grid(
    knn_workflow,
    resamples = telco_folds,
    grid = knn_grid
  )

# Which k is the best?
knn_tune_result %>%
  select_best(metric = "roc_auc") 

# Put the best k in the workflow
knn_tuned <- 
  finalize_workflow(
    knn_workflow,
    parameters = knn_tune_result %>% select_best(metric = "roc_auc")
  )

# Fit the model
fitted_knn <- 
  knn_tuned %>% 
  fit(data = telco_train)

# Predict the test data
predictions_testing <- 
  fitted_knn %>% 
  predict(new_data = telco_test,
          type = "prob") %>% 
  mutate(truth = telco_test$Churn)

# Calculate the AUC
predictions_testing %>% 
           roc_auc(truth, .pred_No)

# Draw the decision boundary
predictions <- 
  predictions %>% 
  mutate(pred_prob_knn = predict(fitted_knn, 
                                 new_data = newdata, 
                                 type = "prob")$.pred_Yes)

scatter +
  geom_contour_filled(aes(x = MonthlyCharges, 
                          y = tenure, 
                          z = pred_prob_knn),
                      data = predictions,
                      inherit.aes = FALSE,
                      breaks = c(0, 0.5, 1)) +
  scale_fill_manual(values = pred_color, 
                    name="Predicted probability", 
                    drop = FALSE) +
  geom_contour(aes(x = MonthlyCharges, 
                   y = tenure, 
                   z = pred_prob_knn),
               data = predictions,
               inherit.aes = FALSE,
               breaks = c(0, 0.5, 1),
               colour = "black",
               size = 1.5)


