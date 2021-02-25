# Session 6: Data Modeling  --------------------------------------------------

# Load library
library(tidyverse)

# Import and load data
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/clean_2016.rds"
tb <- readRDS(url(data_url,"rb"))

# Always look at the data first!
head(tb)


# Last time we ended by checking the relationship between ideology and age
ggplot(tb, aes(x=ideology, y=age)) +
  geom_jitter() +
  theme_light()

# Let's add the line of best fit
ggplot(tb, aes(x=ideology, y=age)) +
  geom_jitter() +
  geom_smooth(method="lm") +
  theme_light()


# Was the relationship positive or negative? TIP: use cor()
tb %>% select(ideology, age) %>% ...

# What is the size of this effect ? TIP: use lm()

# How does lm() work?
# lm(formula, data)
# formula : y ~ x
# data : data.frame

# Let's fit our first model using lm() to answer that question
lm(ideology ~ age, data = tb)

# What about party identification
lm(ideology ~ party_id, data = tb)

# What about education?
lm(ideology ~ education, data = tb)

# What about gender?
lm(ideology ~ gender, data = tb)

# Which model is right?
# All models are wrong but some are useful (Box, 1976)

# Are those effects due to chance or can we be confident about them?
# Let's make a more exhaustive model and check!

lm(ideology ~ age + party_id + education + gender, data = tb)
# This is getting a bit more difficult to understand... bear with me!
# R's fantastic summary (not summarise) function is here to help us.

# Let's save this model
my_model <- lm(ideology ~ age + party_id + education + gender, data = tb)

# We can use R's great summary() function to
summary(my_model)

