# 
# BAN400 - R PROGRAMMING FOR DATA SCIENCE
# LECTURE: MANY MODELS
# 

# In this lecture we will closely follow chapter 25 of Hadley Wickham's book "R
# for data science". He starts the chapter by identifying three learning goals
# with this chapter:
#
#     1. Using many simple models to understand complex data sets.
#     2. Using list-columns to store arbitrary data structures in a data frame.
#     3. Using the broom-package to turn models into tidy data. 

# Load the packages:
library(modelr)                 # Integrate modeling into the workflow
library(tidyverse)              # Data wrangling and visualization
library(broom)                  # Analyzing models
library(gapminder)              # The data set

# Gapminder data ------

#  We will look at the gapminder data set for this tutorial. The data contains
#  country-year-observations of life expectancy, population size and gdp per
#  capita.
gapminder

# The question that we will explore in this tutorial is the following: How does
# life expectancy change over time for each country? We start with a plot:
gapminder %>% 
  ggplot(aes(year, lifeExp, group = country)) +
  geom_line(alpha = 1/3)

# For the most part the life expectancy seems to increase over time, but there
# are exceptions to this rule. We can try to capture the pattern by fitting a
# linear model for each of the countries and then analyze the residuals. For one
# country this is easy:
nz <- filter(gapminder, country == "New Zealand")
nz %>% 
  ggplot(aes(year, lifeExp)) + 
  geom_line() + 
  ggtitle("Full data = ")

nz_mod <- lm(lifeExp ~ year, data = nz)
nz %>% 
  add_predictions(nz_mod) %>%             # This is from the modelr-package.
  ggplot(aes(year, pred)) + 
  geom_line() + 
  ggtitle("Linear trend + ")

nz %>% 
  add_residuals(nz_mod) %>%               # This is also from the modelr-package.
  ggplot(aes(year, resid)) + 
  geom_hline(yintercept = 0, colour = "white", size = 3) + 
  geom_line() + 
  ggtitle("Remaining pattern")

# The problem is that we want to fit this data to every country of the world.
# There are ways to do this with what we have learned earlier in the course in a
# fairly compact way, for example by defining a function that fits the model for
# a given country and extracts the information that we need, and then we can
# perhaps fill an empty data frame using a for-loop. We can do this in a much
# neater way still, and start by creating a nested data frame:
by_country <- 
  gapminder %>% 
  group_by(country, continent) %>% 
  nest()                                   # From the tidyr-package

# Now we have one row per country. Grouping by continent just makes sure that
# the continent variable is not included in the nesting.
by_country

# The data-column contains the measurements for each country, for example:
by_country %>% 
  filter(country == "Afghanistan") %>% 
  ungroup() %>%    
  select(data) %>% 
  pull()

# Let us define a function that fits the model to the country-specific data
# frame, that we can supply to the map()-function afterwards:
country_model <- function(df) {
  lm(lifeExp ~ year, data = df)
}

# Fit the linear model to each country:
models <- map(by_country$data, country_model)

# This returns a model object for each country as an element in a list. But
# given that we are able to store the country specific data-frames in a column
# in a data frame with one country per row, wouldn't it be nice if we can just
# store the model object in the same data frame as well? We can do that easily
# by putting the mapping inside a mutate, that creates this column for us:
by_country <- 
  by_country %>% 
  mutate(model = map(data, country_model))

by_country

# That was convenient! The models are now correctly associated with the
# countries, and we can juggle them around using the normal data wrangling tools
# that we already know. We can easily extract individual models:
by_country %>% 
  filter(country == "Afghanistan") %>% 
  pluck("model", 1) %>%         # Plucking out a list from a data frame {purrr}
  summary
  
# We then want to add the residuals to each of the country-specific data sets.
# We use the map2()-function because we can then specify first the data frame
# and then the column that we are going to loop over:
by_country <- by_country %>% 
  mutate(
    resids = map2(data, model, add_residuals)
  )

# This created a new column of data frames:
by_country

# For plotting, it is now convenient to unnest the results. This results in a
# data frame that has now returns to the one-country-per-year-format (including
# the new residuals), but also with copies of the nested data and the linear
# models in each row. The second argument is the name of the nested column, that
# we created in the previous step.
resids <- unnest(by_country, resids)
resids

resids %>% 
  ggplot(aes(year, resid)) +
  geom_line(aes(group = country), alpha = 1 / 3) + 
  geom_smooth(se = FALSE)

# ... or by continent:
resids %>% 
  ggplot(aes(year, resid, group = country)) +
  geom_line(alpha = 1 / 3) + 
  facet_wrap(~continent)

# The linear model seems to be fine for the most part, especially in the western
# world, but there is a general, but mild "inverse-u-pattern", suggesting that
# the steepness of the trend is generally decreasing. 

# The broom-package contains tools for easily extracting diagnostics from a
# model:
broom::glance(nz_mod)

# We can the apply this to all the models in our data frame, and then unnest:
diagnostics <- 
  by_country %>% 
  mutate(glance = map(model, broom::glance)) %>% 
  select(-data,-model, -resids) %>% 
  unnest(glance)

# We can then start to look for models that do not fit very well:
diagnostics %>% 
  arrange(r.squared)

# The worst fits seem to be for African countries:
diagnostics %>% 
  ggplot(aes(x = continent, y = r.squared, colour = continent)) + 
  geom_jitter(width = 0.3)

# We can pull out the countries with the worst fit, and look at them
# specifically:
bad_fit <- filter(diagnostics, r.squared < 0.25)

gapminder %>% 
  semi_join(bad_fit, by = "country") %>%            # This is a *filter*
  ggplot(aes(year, lifeExp, colour = country)) +
  geom_line()

# The author of the book identifies two main effects here: the HIV/AIDS epidemic
# and the Rwandan genocide.

# The rest of the chapter goes into a bit more details on various ways to create
# and work with tibble columns containing lists (data frames, models, etc...).
# Pay attention to the three parts of an effective list-column pipeline:
# 
#     1. You create the list-column using one of nest(), summarise() + list(), 
#        or mutate() + a map function.
#     2. You create other intermediate list-columns by transforming existing 
#        list columns with map(), map2() or pmap().
#     3. You simplify the list-column back down to a data frame or atomic 
#        vector.
#



