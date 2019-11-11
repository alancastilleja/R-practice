library(dplyr)
library(NHANES)
data(NHANES)
# filtering by gender and age, %in% asks if what we are asking for is in the vector
tab <- NHANES %>% filter(Gender %in% c("female") & AgeDecade == " 20-29")
head(tab)
head(NHANES)

## mean and standard deviation
ref <- NHANES %>% filter(AgeDecade == " 20-29" & Gender == "female") %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))
ref

?pull
## only pulls average
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>% .$average
ref_avg

## min and max
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>% summarize(minbp = min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm = TRUE))

##complete the line with group_by and summarize
NHANES %>%
  filter(Gender == "female") %>% 
  group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# summarizing by age and gender 
NHANES %>% 
  filter(Gender == "male") %>% 
  group_by(AgeDecade) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# filtering without filter function
NHANES %>% 
  group_by(AgeDecade, Gender) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

# filtering by race and arranging by average BPSyncAve in descending order (done on my own)
NHANES %>% 
  filter(Gender == "male" & AgeDecade == " 40-49")%>% 
  group_by(Race1) %>% 
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE)) %>% 
  arrange(desc(average))
