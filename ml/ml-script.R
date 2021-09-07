
# ------------------------------------ #
# BAN400 - SESSION ON MACHINE LEARNING #
# ------------------------------------ #

library(dplyr)
library(ggplot2)
library(tidymodels)      # <- This is new. Tidyverse take on modeling

no_color <- "#00000020"
yes_color <- "#FF000070"
pred_color <- c("#FFFFFF00", "#AA3333DD")
cutoff <- .5
grid_resolution <- 50

# Read the data
telco <- 
  readr::read_csv("telco-customer-churn/WA_Fn-UseC_-Telco-Customer-Churn.csv") %>% 
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
                         to =80, 
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
                    drop = FALSE) 

# Do a prediction using k-nearest-neighbours (kNN)
# Requires package: "kknn"
fitted_knn <-
  nearest_neighbor(neighbors =30) %>% 
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
                    drop = FALSE) 
