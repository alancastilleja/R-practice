library(dslabs)
data(heights)
options(digits = 3)
head(heights)

# question 1 filtering who is above average height
mean(heights$height)
filter(heights, height >= 68.3)
ind <- heights$height > mean(heights$height)
sum(ind)

#question 2 number of females who are above average height
ind2 <- filter(heights, sex %in% c("Female") & height >= 68.3)
nrow(ind2)


#question 3 proportion of data set who is female
summary(heights)
mean(heights$sex == "Female")

# question 4a
min(heights$height)

# question 4b matching index to heights 
short <- min(heights$height)
short
ind4 <- match(short, heights$height)
ind4

# question 4c subsetting data 
heights$sex[ind4]

# question 5 max height
max(heights$height)

# question 5b integers between min and max height
x <- 50:82
x

# question 5c which integers are not between the min and max
y <- x %in% heights$height
sum(!y)

# question 6 adding height in centimeters column
heights2 <- mutate(heights, ht_cm = 2.54 * height)
summary(heights)

# question 6a 
heights$ht_cm[18]

#question 6b finding the mean
summmary(heights)

#question 7a
females2 <- filter(heights2, sex == "Female")
nrow(females2)

# my way of solving it
females <- data.frame(heights2$sex == "Female", heights2$ht_cm)
summary(females)
mean(females$heights2.ht_cm)

# question 7b
mean(females2$ht_cm)
