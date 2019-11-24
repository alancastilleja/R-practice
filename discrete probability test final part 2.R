library(gtools)
library(tidyverse)
library(dplyr)
data("esoph")

head(esoph)
str(esoph)

# Question 3a, number of groups
nrow(esoph)

# Question 3b, number of cases
all_cases <- sum(esoph$ncases)
all_cases

# Question 3c, number of controls (no cancer)
all_controls <- sum(esoph$ncontrols)
all_controls

# Question 4a probability of highest alcgroup having cancer
total_individuals <- all_cases + all_controls
total_individuals

only_75 <- filter(esoph, agegp == "75+") # extra stuff
nrow(only_75)
highest_alcgp <- filter(esoph, alcgp == "120+")
sum(highest_alcgp$ncases)
sum(highest_alcgp$ncontrols)
highest_alcgp_individuals <- sum(highest_alcgp$ncases) +
  sum(highest_alcgp$ncontrols)
highest_alcgp_individuals

sum(highest_alcgp$ncases) / highest_alcgp_individuals

esoph %>% filter(alcgp == "120+") %>% 
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_cases = ncases / (ncases + ncontrols)) %>% .$p_cases

# Question 4B
esoph$alcgp
esoph %>% filter(alcgp == "0-39g/day") %>%
  summarize(ncases = sum(ncases), ncontrols = sum(ncontrols)) %>%
  mutate(p_cases = ncases / (ncases + ncontrols)) %>% .$p_cases

# Question 4c
smokers_with_case <- esoph %>% filter(tobgp >= "10-19" & ncases > 0) %>% summarise(smokers_case = sum(ncases))
smokers_with_case / all_cases

# Question 4d
smokers_no_case <- esoph %>% 
  filter(tobgp >= "10-19" & ncontrols > 0) %>% 
  summarise(smokers_nocase = sum(ncontrols))
smokers_no_case
smokers_no_case / all_controls

tob_controls <- esoph %>%
  filter(tobgp != "0-9g/day") %>%
  pull(ncontrols) %>%
  sum()

# Question 5A Given case, prob of being in high alc group
high_alc_cases <- esoph %>% 
  filter(ncases > 0 & alcgp == "120+") %>% 
  summarize(ncases = sum(ncases))
high_alc_cases

prob_alc <- high_alc_cases / all_cases

# Question 5B
high_tob_cases <- esoph %>% filter(ncases > 0 & tobgp == "30+") %>%
  .$ncases %>% sum()
high_tob_cases

prob_tob <- high_tob_cases / all_cases

# Question 5C
high_tob_and_alc_cases <-  esoph %>% 
  filter(alcgp == "120+" & tobgp == "30+") %>% .$ncases %>% sum()

prob_and <- high_tob_and_alc_cases / all_cases

# Question 5D
or_cases <- prob_alc + prob_tob - prob_and

# Question 6A
high_alc_control <- esoph %>% 
  filter(alcgp == "120+") %>% 
  summarize(ncontrol = sum(ncontrols))
high_alc_control

control_alc <- high_alc_control / all_controls

# Question 6B how many times more likely are cases than controls to be in the high alc group
prob_alc / control_alc

# Question 6C
high_tob_control <- esoph %>% filter(tobgp == "30+") %>%
  .$ncontrols %>% sum()
high_tob_control

control_tob <- high_tob_control / all_controls
control_tob

# Question 6D
high_tob_and_alc_control <- esoph %>% 
  filter(alcgp == "120+" & tobgp == "30+") %>% .$ncontrols %>% sum()

prob_and_control <- high_tob_and_alc_control / all_controls
prob_and_control

# Question 6E
or_control <- control_tob + control_alc - prob_and_control

# Question 6F
or_cases / or_control
