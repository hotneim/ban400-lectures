# Exercises for the many models lesson.
# 
# 1. (Basic) Exercises 1-3 under 25.2.5 in R4DS
# 2. (Basic-ish) The Jackknife problem from early BAN420.
# 3. (Challenging) The all-combinations problem from early BAN420.

library(tidyverse)
library(readr)
library(tidymodels)
library(purrr)

# Read in the data from the file "growth.csv". This is a small data set
# covering income per capita and a few other variables in a cross section of
# countries. Make sure the formatting of the data is sensible. Drop the
# variables lbnppc_2014 and skole_p_1996. Thereafter, drop all observations with
# any missing observations. (You should have 94 obs. after this). Create a
# dependent variable "growth", defined as (lbnppc_1996-lbnppc_1960)/(1996-1960).
# This is the average annual growth rate of gdp per capita in the time period
# 1960 to 1996. Estimate a regression model with growth as a dependent variable,
# and initial income (lbnppc_1960) and the initial level of mining operations in
# the country (gruve_1960) as independent variables.

countries <- 
  read_delim("growth.csv", delim = ";") %>%
  mutate(growth = (lbnppc_1996-lbnppc_1960)/(1996-1960)) %>%
  select(-lbnppc_2014, -skole_p_1996) %>%
  drop_na

# The basic regression. None of the variables are significant. 
summary(lm(growth ~ lbnppc_1960 + gruve_1960, data = countries))

# 2.Calculate the Jackknife estimates of the regression coefficients. A
# jackknife estimate is the following: There are N observations. Estimate the
# regression model N times, omitting one observation at the time. The jackknife
# estimate is the mean of the N regression coefficient estimates. Also, show a
# histogram over the N regression coefficients. Do *not* use any for-loops. Make
# a visual representation of the mining-coefficient and interpret the results.

# The strategy here is to make a data frame in the same way as we did in the
# lecture, where we for each country store the data set *wihout* that particular
# country.

drop_country <- function(drop) {
  countries %>% filter(land != drop)
}

fit_regression <- function(dat) {
  lm(growth ~ lbnppc_1960 + gruve_1960, data = dat)
}

jackknife_coef <- 
  countries %>% 
  mutate(reg_data = map(land, drop_country)) %>% 
  mutate(model = map(reg_data, fit_regression)) %>% 
  mutate(coeff = map(model, tidy)) %>% 
  unnest(coeff) %>% 
  select(landkode, land, term, estimate)

# Calculate the jackknife estimates of the coefficients:
jackknife_coef %>% 
  group_by(term) %>% 
  summarize(jackknife_estimate = mean(estimate))

# The jackknife estimates of the mining coefficient is approximately the same as
# the normal estimate. But by looking at the historam of the estimated mining
# coefficient we see something strange:
jackknife_coef %>% 
  filter(term == "gruve_1960") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram()

# It looks like there is one country that completely drives the results. If we
# take out the single country, the estimated coefficient is strongly negative.
# Otherwise it is very positive. (Can you figure out what country that is and
# try to explain the results?)

# 3. This one is pretty tricky! We want to estimate the effect of initial income
# (lbnppc_1960) on growth. However, we don't know which other six variables (i.e
# "skole_p_1960", "skole_h_1960", "gruve_1960", "olje_1960", "malaria_1960" and
# "kapitalisme_1960") should be included in the regression. Maybe none? Maybe
# one? Maybe two? Three? Four? Five? Or all of them? There are in total 2^6=64
# possible regressions. Run all of them, and plot a histogram/density over the
# estimated coefficient on initial income. (This is a teaser to so-called "Model
# averaging". We usually weigh together results based on how well the models fit
# the data, with some penalty for larger models). Hint: The functions combn or
# expand.grid may be useful.

# Here are the possible variables
possible_variables <- c("skole_p_1960", "skole_h_1960", "gruve_1960", 
                        "olje_1960", "malaria_1960", "kapitalisme_1960")

# Try the functions combn to create all possible combinations of m elements
# drawn from a vector:
combn(possible_variables, m = 3, simplify = TRUE)

# We need to collect all of these for m = 1:6. We try the map-function to avoid
# the for loop. This was not very easy to figure out, look at each step and see 
# if you get it.

# Start by creating a simple data frame containing the numbers from 0 to 6,
# which are what we are looking to loop over in the combn-functionn:
estimates <- 
  tibble(n = 0:6) %>% 
  # We then map the combn-function to the n-column. Note that we do not writ the
  # helper function separately, we just define it directly in the map-fundtion.
  mutate(combinations = map(n, function(x) combn(possible_variables, 
                                                 m = x, 
                                                 simplify = FALSE))) %>% 
  # Unpack the combinations, giving us one entry per possible combination of the
  # variables.
  unnest(combinations) %>% 
  # We use the map function to add the final variable that should be present in
  # all of the regressions. We also need the response variable:
  mutate(combinations = map(combinations, 
                            function(x) c(x, "lbnppc_1960", "growth"))) %>% 
  # Make a new column with a version of the data set that has been subsetted
  # accoring to the character vector of variables in the combinations-column:
  mutate(reg_data = map(combinations, function(x) countries %>% select(x))) %>% 
  # Run the regression on all of the data sets:
  mutate(model = map(reg_data, function(x) lm(growth ~ ., data = x))) %>% 
  # Unpack like we did above:
  mutate(coeff = map(model, tidy)) %>% 
  unnest(coeff) %>% 
  # Select the estimates. We can keep more variables if we are interested in
  # looking more into which estimate comes from where. It is also advisable to
  # collect the standard deviations of the estimates in order to et an idea of
  # estimation uncertainty.
  select(term, estimate)

# Plot a histogram of all of the estimates of the initial wealth coefficients:
estimates %>% 
  filter(term == "lbnppc_1960") %>% 
  ggplot(aes(x = estimate)) +
  geom_histogram() +
  ylab("") + xlab("Estimate of the coefficient of 'lbnppc_1960'") +
  theme_bw()
