library(dplyr)
library(dslabs)
# mutate
str(murders)
murders <- mutate(murders, rate = total/population*100000)
murders
head(murders)
# filter
filter(murders,rate <= 0.71)
# select
new_table <- select(murders, state,region,rate)
filter(new_table, rate <= 0.71)

# using the pipe function. %>% can be seen as "and then"
murders %>% select(state,region,rate) %>% filter(rate <= 0.71)
?nrow
nrow(new_table)
str(murders)
