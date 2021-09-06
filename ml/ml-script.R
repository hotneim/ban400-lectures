
# ------------------------------------ #
# BAN400 - SESSION ON MACHINE LEARNING #
# ------------------------------------ #

library(dplyr)
library(ggplot2)
library(tidymodels)      # <- This is new. Tidyverse take on modeling.

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
  scale_color_manual(values=c("#00000030", "#FF0000")) +
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
