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

# TIP: Always check your variables using exploration basics!
# class()    returns the class of an object.
# dim()      returns the dimension of an object.
# head()     returns the first elements of an object.
# names()    returns the names of an object.
# str()      returns the structure of an object.
# table()    returns the counts at each combination.
# summary()  returns the result summaries of an object.

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
# Keep only respondents who casted a vote in 2012
dataset %>%
  filter(V161005 == 1)

# Keep respondents below 25 years old
...
dataset %>%
  filter(V161267 < 25)

# Keep respondents below 25 years who are not missing and casted a vote
...
tmp_data <- dataset %>%
  filter(between(V161267, 1, 25))

# Select the previous 5 variables,
# filter the age variable by removing missing values
# and store the output into a new object called `tmp_data`
...
tmp_data <- dataset %>%
  select(V161005, V161267, V161126, V161031, V161155) %>%
  filter(between(V161267, 1, 98))


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
# ...
table(young_voters$V161031)
young_voters %>%
  mutate(V161031 = V161031 == 1)

# Create a new variable/column
...
table(young_voters$V161031)
young_voters %>%
  mutate(vote_dem = V161031 == 1) %>%
  select(vote_dem) %>%
  table

# Combine select, filter, mutate, and arrange
...

# The Rename Verb ##############################################################
# ...
young_voters %>%
  rename(vote_intention = V161031)

# Rename V161005 with a meaningful name
...

# Create meaningful names for each of the 5 variables in tmp_data and save them.
...
tmp_data <- tmp_data %>%
  rename(
    vote_part  = V161005,
    age        = V161267,
    left_right = V161126,
    vote_int   = V161031,
    party_id   = V161155
  )


# Data Visualization ===========================================================
# ggplot basic functions #######################################################

# ...
ggplot(dataset, aes(V161267)) +
  geom_bar()

# Filter the data and plot again
...
ggplot(filter(dataset, between(V161267, 1, 90)), aes(V161267)) +
  geom_bar()

# Add meaningful x and y axis labels
...
ggplot(filter(dataset, between(V161267, 1, 90)), aes(V161267)) +
  geom_bar() +
  labs(x="test", y="test2")

# Make a barplot of the ideological self-placement variable
...
ggplot(filter(tmp_data, between(left_right, 1,7)), aes(left_right)) +
  geom_bar()

# Explore the relationship between ideology and age using a scatter plot.
...
ggplot(filter(tmp_data, between(left_right, 1,7)), aes(x=left_right, y=age)) +
  geom_point()

# Explore the relationship between ideology and age using box plots.
# WARNING: We will need to fix the variable type!
...
ggplot(filter(tmp_data, between(left_right, 1,7)), aes(x=as.factor(left_right), y=age)) +
  geom_boxplot()

# Explore the relationship between age and voting participation
...
ggplot(filter(tmp_data), aes(x=as.factor(vote_part), y=age)) +
  geom_boxplot()

# Add meaningful x- and y-axis labels
...


# Data Summaries ===============================================================

tmp_data
