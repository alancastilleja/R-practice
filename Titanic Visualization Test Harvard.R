options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(dslabs)
install.packages("titanic")
library(titanic)
str(Titanic)
str(titanic_train)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))
titanic
class(titanic$Survived)

# Question 1
?titanic_train
titanic_train %>% max(Age, na.rm = TRUE)

# Question 2 - density plot for male and female pop/ages

# filtering NAs out
titanic_plot <- titanic %>% filter(!is.na(Age)) 

# creating geom_density plot with count
titanic_plot %>% ggplot(aes(Age, ..count.., fill = Sex)) + 
  geom_density(position = "stack")

# creating geom_density plot with count and facet_grid
titanic_plot %>% ggplot(aes(Age, ..count.., fill = Sex)) + 
  geom_density(position = "stack") + facet_grid(.~Sex)

# with density
titanic_plot %>% ggplot(aes(Age, fill = Sex)) + geom_density(position = "stack") + facet_grid(.~Sex)

#box plot comparing ages under 17 to see oldest in the set
titanic_plot  %>% ggplot(aes(Sex, Age, fill = Sex)) + geom_boxplot()
?geom_boxplot

# females and male over 40 on density plot
titanic_plot %>% filter(Age >= 40) %>% 
  ggplot(aes(Age, ..count.., fill = Sex)) + 
  geom_density(position = "stack") 
head(titanic_plot)
summary(titanic_plot)
?facet_grid

# proportions of females and males within certain age range (basically checking my work if it matched the graph)


##female under 17
titanic_females_under_17 <- titanic_plot %>% filter(Age <= 17 & Sex == "female") %>% summarize(total = sum(as.numeric(Sex)))
titanic_females <- titanic_plot %>% filter(Sex == "female") %>% summarize(total = sum(as.numeric(Sex)))
titanic_female_proportion_under_17 <- titanic_females_under_17 / titanic_females
titanic_female_proportion_under_17

## males under 17
titanic_males_under_17 <- titanic_plot %>% 
  filter(Age <= 17 & Sex == "male") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_males <- titanic_plot %>% filter(Sex == "male") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_male_proportion_under_17 <- titanic_males_under_17 / titanic_males
titanic_male_proportion_under_17

## males 18 to 35
titanic_males_18_35 <- titanic_plot %>% 
  filter(Age >= 18 & Age <= 35 & Age & Sex == "male") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_male_proportion_18_35 <- titanic_males_18_35/ titanic_males
titanic_male_proportion_18_35

## females 18 to 35
titanic_females_18_35 <- titanic_plot %>% 
  filter(Age >= 18 & Age <= 35 & Age & Sex == "female") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_female_proportion_18_35 <- titanic_females_18_35/ titanic_females
titanic_female_proportion_18_35

## females over 40
titanic_females_40 <- titanic_plot %>% 
  filter(Age >= 40 & Age & Sex == "female") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_females_40

# males over 40
titanic_males_40 <- titanic_plot %>% 
  filter(Age >= 40 & Age & Sex == "male") %>% 
  summarize(total = sum(as.numeric(Sex)))
titanic_males_40

# Question 3 - qq plot for passanger age
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
?geom_qq

titanic %>% ggplot(aes(sample = Age)) + geom_qq(dparams = params) + geom_abline()


# Question 4

# bar plot showing who survived and who did not based on sex
titanic_plot %>% ggplot(aes(Survived, fill = Sex)) + geom_bar(position = position_dodge())

# finding proportions

# males survived
males_survived <- titanic_plot %>% filter(Sex == "male" & Survived == "1") %>% count(Survived) 
males_survived
total_males <- titanic_plot %>% filter(Sex == "male") %>% count(Sex)
total_males
proportion_m <- males_survived / total_males
proportion_m

#females survived
females_survived <- titanic_plot %>% filter(Sex == "female" & Survived == "1") %>% count(Survived)
females_survived
total_females <- titanic_plot %>% filter(Sex == "female") %>% count(Sex)
total_females
proportion_f <- females_survived / total_females
proportion_f
?titanic_train

# total survived
total_survived <- titanic_plot %>% filter(Survived == "1") %>% count(Survived)
total_survived  

total_on_titanic <- length(titanic_plot[[7]])
total_on_titanic

proportion_total <- total_survived / total_on_titanic
proportion_total

# Question 5
titanic_plot %>% ggplot(aes(Age, ..count.., fill = Survived)) + 
  geom_density(position = "stack", alpha = 0.2) 

# density instead of count
titanic_plot %>% ggplot(aes(Age, fill = Survived)) + 
  geom_density(position = "stack", alpha = 0.2)

#faceted
titanic_plot %>% ggplot(aes(Age, fill = Survived)) + 
  geom_density(position = "stack", alpha = 0.2) + facet_grid(.~Survived)

titanic_plot %>% ggplot(aes(Age, ..count.., fill = Survived)) + 
  geom_density(position = "stack", alpha = 0.2) + facet_grid(.~Survived)

# all the old people died

# Question 6
titanic_boxplot <- titanic_plot %>% filter(Fare != 0)
titanic_boxplot %>% ggplot(aes(Survived, Fare, fill = Survived)) + geom_boxplot() + geom_jitter(width = .1, alpha = 0.2, height = 0.2) + scale_y_continuous(trans = "log2")
titanic_plot %>% filter(Fare >= 400 & Fare != 0 & Survived == "1") 

# Question 7

## barplot 1 shows counts of people in each class and whether they survived 
barplot_1 <- titanic_plot %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = position_stack())
barplot_1
?geom_bar

## barplot 2 shows proportions of who died or not in each class
barplot_2 <- titanic_plot %>% ggplot(aes(Pclass, fill = Survived)) + geom_bar(position = position_fill())
barplot_2

## barplot 3 shows proportions of survival by passanger class 
barplot_3 <- titanic_plot %>% ggplot(aes(Survived, fill = Pclass)) + geom_bar(position = position_fill())
barplot_3

# extra work
add1 <- titanic_plot %>% filter(Pclass == "1") %>% count(Pclass)
add2 <- titanic_plot %>% filter(Pclass == "2") %>% count(Pclass)
add3 <- titanic_plot %>% filter(Pclass == "3") %>% count(Pclass)
titanic_plot %>% group_by(Pclass, Survived) %>% count(Survived)
add3 < add1 + add2
add3
add1 + add2
add3 > add2 
add2 > add1
?min

# Question 8
titanic_plot %>% ggplot(aes(Age, ..count.., fill = Survived)) + 
  geom_density(position = "stack", alpha = 0.2) + facet_grid(Pclass~Sex)
