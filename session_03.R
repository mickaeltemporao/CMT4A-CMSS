# Session 3: Data Management 1/2 -----------------------------------------------

# Data Wrangling ===============================================================

# Getting Started ##############################################################
# Load the tidyverse package
library(tidyverse)
...

# ...
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/blob/main/data/ts_2016.rds"

# ...
if(!file.exists("ts_2016.rds")) {
  download.file(data_url, "ts_2016.rds")
}

# ...
dataset <- readRDS("ts_2016.rds")

# Check the dataset
# class, head, dim, etc.
...


# The Select Verb & The Pipe ###################################################

# ...
dataset %>%
  select(V161005, V161267, V161126, V161031, V161155)

# Whare are theses variables?
...

# Select a single variable
...

# Select the previous 5 variables and store them into a new object called `tmp_data`
...
tmp_data <- dataset %>%
  select(V161005, V161267, V161126, V161031, V161155)

# The Filter Verb ##############################################################
# Keep only respondents who voted in 2012
dataset %>%
  filter(V161005 == 1)

# Keep respondents below 25 years old
...
dataset %>%
  filter(V161267 < 25)

# Keep respondents below 25 years who are not missing and who voted
...
tmp_data <- dataset %>%
  filter(between(V161267, 1, 25))

# Select the previous 5 variables, filter them and store them into a new object called `tmp_data`
...
tmp_data <- dataset %>%
  select(V161005, V161267, V161126, V161031, V161155) %>%
  filter(between(V161267, 1, 25))


# The Arrange Verb #############################################################
# Sort respondents by age
tmp_data %>%
  arrange(V161267)

# Select 5 variables, filter non-missing young voters and arrange observations by age
...

# Store the output in a new object called young_voters
...
young_voters <- tmp_data %>%
  arrange(V161267)

# Return young voters in descending order
...
tmp_data %>%
  arrange(desc(V161267))


# The Mutate Verb ##############################################################

# TIP: Always check your variables using exploration basics!
# head()     returns the first elements of the object.
# names()    returns the names of an object.
# str()      returns the structure of an object.
# table()    returns the counts at each combination.
# summary()  returns the result summaries of the object.

# Change an existing column/variable
...
table(young_voters$V161126)
young_voters %>%
  mutate(V161126 = ifelse(V161126 < 4, "lib", V161126)) %>%
  select(V161126) %>%
  table

# Create a new variable/column
...
table(young_voters$V161031)
young_voters %>%
  mutate(vote_dem = V161031 == 1) %>%
  select(vote_dem) %>%
  table

# Combine select, filter, mutate, and arrange
...

# Data Visualization ===========================================================



