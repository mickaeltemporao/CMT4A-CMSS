# Session 4: Data Management  --------------------------------------------------

# Getting Started ==============================================================
# Load the tidyverse package
library(tidyverse)

# Identify the path to the `ts_2016.rds` dataset on your machine
data_path <- "../template/data/ts_2016.rds"

# Read your data and save it into an object called `dataset`
dataset <- readRDS(data_path)

# Let's work on a subset of the dataset using meaningful names
tmp_data <- dataset %>%
  select(
    V163003,   # region
    V161031,   # vote_intent
    V161086,   # ft_democrat
    V161087,   # ft_republican
    V161126,   # ideology
    V161155,   # party_id
    V162034a   # actual_vote
  ) %>%
  rename(
    "region"        = "V163003",
    "vote_intent"   = "V161031",
    "ft_democrat"   = "V161086",
    "ft_republican" = "V161087",
    "ideology"      = "V161126",
    "party_id"      = "V161155",
    "actual_vote"   = "V162034a"
  )

# Let's focus on the voting intention variable for now.
# Peek at the variable in the newly created dataset
head(tmp_data)

# How would you do this using the tidyverse pipes?
tmp_data %>%
  head

# Plot the vote_intent variable
  ggplot(tmp_data, aes(x = vote_intent)) +
  geom_bar()

# What's wrong with this variable?

# The summary function can also help to see flaws in the data.
summary(tmp_data)


# Data Cleaning ===============================================================
# Handle missing values and save it into a new object
clean_data <- tmp_data %>%
  filter(vote_intent > 0)

# Plot gain
ggplot(clean_data, aes(x = vote_intent)) +
geom_bar()

# Note that a whole course could be given on how to handle missing values.
# For now we are just going to filter out those observations.

# What is the current class of this variable?
class(clean_data$vote_intent)

# Is this class appropriate for this variable? If no, change its the type.
clean_data <- tmp_data %>%
  filter(vote_intent>0) %>%
  mutate(vote_intent = as.character(vote_intent))

# Let's look again
ggplot(clean_data, aes(x=vote_intent)) +
geom_bar()

# What do the 1-8 numbers mean? Keep only what's relevant.
clean_data <- tmp_data %>%
  filter(vote_intent > 0, vote_intent < 5) %>%
  mutate(vote_intent = as.character(vote_intent))

# Look again
ggplot(clean_data, aes(x=vote_intent)) +
geom_bar()

# Recode the values of the variable
clean_data <- tmp_data %>%
  filter(vote_intent > 0, vote_intent < 5) %>%
  mutate(vote_intent = as.character(vote_intent)) %>%
  mutate(
    vote_intent = recode(
      vote_intent,
      "1" = "Clinton",
      "2" = "Trump",
      "3" = "Johnson",
      "4" = "Stein"
    )
  )

# Look again
ggplot(clean_data, aes(x=vote_intent)) +
geom_bar()


# Answering quesitons with group_by and summarise ==============================

# How many respondents intend to vote for each candidate?
# This is where `groub_by()` and `summarise()` will come in handy!
clean_data %>%
  group_by(vote_intent) %>%
  summarise(freq = n())

# What is the predicted vote share for each candidate using voting intentions?
clean_data %>%
  group_by(vote_intent) %>%
  summarise(freq = n()) %>%
  mutate(
    prop = freq/sum(freq)
  )

# How good are those numbers relative to the actual numbers?
# Check Wikipedia for the actual numbers and compare.

# Note that if we want to reuse the information we should save it!
# Let's do that.

vote_intent_summary <- clean_data %>%
  group_by(vote_intent) %>%
  summarise(freq = n()) %>%
  mutate(
    prop = freq/sum(freq)
  )


# Plotting data summaries ======================================================

# Now let's make an actual plot with the percentages
ggplot(vote_intent_summary, aes(x=vote_intent)) +
geom_bar()

# What's wrong here?

# Try again using geom_col() instead of geom_bar
ggplot(vote_intent_summary, aes(x=vote_intent, y=prop))+
geom_col()

# Let's quickly clean liberal-conservative self placement variable.
clean_data <- clean_data %>%
  filter(
    ideology > 0,
    ideology < 8
  ) %>%
  mutate(
    ideology = as.numeric(ideology)
  )

# Peek at the variables
clean_data %>%
  select(vote_intent, ideology) %>%
  # WARNING: summary() is a different function summarise()!
  summary()

# What is the average (mean) ideology per vote intention?
clean_data %>%
  group_by(vote_intent) %>%
  summarise(
    avg = mean(ideology)
  )

# Do these result seem valid?

# Now try to find which region is the most conservative?
clean_data %>%
  group_by(...) %>%
  ...(avg_idl = mean(...)) %>%
  arrange(...)

# Let's try to summarise the ideology per vote intention visually
# We need to save the intermediary averages
idl_means <- clean_data %>%
  group_by(vote_intent) %>%
  summarise(
    avg = mean(ideology)
  )

# And then we make a plot
ggplot(clean_data, aes(x = ideology, y = ..prop.., group=vote_intent)) +
  geom_bar() +
  facet_wrap(~vote_intent) +
  geom_vline(
    data=idl_means,
    aes(xintercept=avg), color="red", linetype=2) +
  scale_x_discrete(
    limits=c("Ext. liberal", "", "", "Moderate", "", "", "Ext. conservative")
  ) +
  labs(
    x = "Self placement",
    y = "Proportion"
  ) +
  theme_bw()

# Can you summarise the ideology per region in a figure?
...

