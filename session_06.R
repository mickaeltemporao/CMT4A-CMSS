# Session 6: Data Modeling  --------------------------------------------------

# Load library
library(tidyverse)

# Import and load data
data_url <- "https://github.com/mickaeltemporao/CMT4A-CMSS-TEMPLATE/raw/main/data/clean_2016.rds"
tb <- readRDS(url(data_url,"rb"))

# Always look at the data first!
head(tb)

#
tb %>% select(ideology, age) %>% cor()

summary(lm(ideology ~ age, data = tb))

add gender control

ggplot(tb, aes(x = ideology, y = ..prop.., group=voting_int)) +
  geom_bar() +
  facet_wrap(~voting_int) +
  geom_vline(
    data=idl_means,
    aes(xintercept=avg), color="red", linetype=2) +
  geom_text(
    data=idl_means,
    aes(x=avg, y=0.25, label=round(avg, 2)), color="red", hjust=-0.25
    ) +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  ) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()


# How is ideology distributed among people for which religion is important?
ggplot(tb, aes(x = ideology, y = ..prop.., group=religion)) +
  geom_bar() +
  facet_wrap(~religion) +
  geom_vline(
    data=tb %>% group_by(religion) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(xintercept=avg), color="red", linetype=2) +
  geom_text(
    data=tb %>% group_by(religion) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(x=avg, y=0.25, label=round(avg, 2)), color="red", hjust=-0.25
    ) +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  ) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()


# How do relgious people intend to vote?
ggplot(tb, aes(x = voting_int, y = ..prop.., group=religion)) +
  geom_bar() +
  facet_wrap(~religion)


# Do right-wing people approve president Obama?
ggplot(tb, aes(x = ideology, y = ..prop.., group=pres_appr)) +
  geom_bar() +
  facet_wrap(~pres_appr) +
  geom_vline(
    data=tb %>% group_by(pres_appr) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(xintercept=avg), color="red", linetype=2) +
  geom_text(
    data=tb %>% group_by(pres_appr) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(x=avg, y=0.25, label=round(avg, 2)), color="red", hjust=-0.25
    ) +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  ) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()


# Are people who express sexist attitudes left-wing or right-wing?
ggplot(tb, aes(x = ideology, y = ..prop.., group=sexism)) +
  geom_bar() +
  facet_wrap(~sexism) +
  geom_vline(
    data=tb %>% group_by(sexism) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(xintercept=avg), color="red", linetype=2) +
  geom_text(
    data=tb %>% group_by(sexism) %>% summarise(avg=mean(ideology)) %>% arrange(avg),
    aes(x=avg, y=0.25, label=round(avg, 2)), color="red", hjust=-0.25
    ) +
  scale_x_discrete(
    limits=c("Extrm. Lib", "", "", "Moderate", "", "", "Extrm. Con.")
  ) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()


names(tb)



# Are young people more right or left wing?
ggplot(tb, aes(x=factor(ideology), y=age)) +
  geom_boxplot() +
  facet_wrap(~latino) +
  geom_vline(
    data=tb %>% group_by(latino) %>% summarise(avg = mean(ideology)) %>% arrange(avg),
    aes(xintercept=avg), color="red", linetype=2) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()


# Is there a difference among Latinos?
ggplot(tb, aes(x=factor(ideology), y=age)) +
  geom_boxplot() +
  facet_wrap(~latino) +
  geom_vline(
    data=tb %>% group_by(latino) %>% summarise(avg = mean(ideology)) %>% arrange(avg),
    aes(xintercept=avg), color="red", linetype=2) +
  labs(
    x = "Ideological Self placement",
    y = "Proportion"
  ) +
  theme_light()

# How were citizens feeling towards Democrats?
daily_feeling <- tb %>%
  group_by(date) %>%
  summarise(
    Democrat=mean(feeling_dem),
    Republican=mean(feeling_rep)
  )

ggplot(daily_feeling, aes(x = date)) +
  geom_smooth(aes(y = Democrat), span=.2, se=FALSE, color="#0B53C1") +
  geom_smooth(aes(y = Republican), span=.2, se=FALSE, color="#FF0055") +
  labs(
    title = "Average daily feeling towards major political parties",
    x = "",
    y = "Feelings Thermometer",
    caption = "Data: ANES 2016"
  ) +
  theme_minimal()
