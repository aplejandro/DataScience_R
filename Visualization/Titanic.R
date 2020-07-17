install.packages("titanic")
library(titanic)
library(tidyverse)
options(digits = 3)    # report 3 significant digits

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
total <- titanic %>%
  filter(Sex %in% c("female","male")) %>%
  count(Sex)
total

age40 <- titanic %>%
  filter(Age == 40 & Sex %in% c("female","male")) %>%
  count(Sex)
age40

max_age <- titanic %>%
  filter(!is.na(Age)) %>%
  group_by(Sex) %>%
  summarize(max = max(Age))
max_age

age17 <- titanic %>%
       filter(Age <= 17 & Sex %in% c("female","male")) %>% count(Sex)
age17/total

age35 <- titanic %>%
  filter(Age >= 18 & Age <=35 & Sex %in% c("female","male")) %>% count(Sex)
age35/total

titanic %>%
  filter(!is.na(Age)) %>%
  ggplot(aes(x = Age, group = Sex, fill = Sex)) + 
  geom_density(alpha = 0.2, bw = 10) 

#Q3

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))

p <- titanic %>%
  ggplot(aes(sample = Age)) +
  geom_qq(dparams = params) +
  geom_abline()
p

#Q4
p <- titanic %>%
  ggplot(aes(x = Survived, fill = Sex)) +
  geom_bar(position = position_dodge())
p

#Q5
p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2)
p

#Q6
p <- titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(Survived, Fare)) +
  geom_boxplot(alpha = 0.2) +
  scale_y_continuous(trans = "log2") +
  geom_point(show.legend = FALSE) + 
  geom_jitter()
p 

#Q7
p <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar()
p

p <- titanic %>%
  ggplot(aes(x = Pclass, fill = Survived)) +
  geom_bar(position = position_fill())
p

p <- titanic %>%
  ggplot(aes(x = Survived, fill = Pclass)) +
  geom_bar(position = position_fill())
p

#Q8
p <- titanic %>%
  ggplot(aes(x = Age, y = ..count.., fill = Survived)) +
  geom_density(alpha = 0.2) +
  facet_grid(Pclass ~ Sex)
p