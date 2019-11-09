name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

options(digits = 3)
time_in_hours <- time * 1/60
time_in_hours

mph  <- distance / time_in_hours
mph

names(name) <- distance
name


names(name) <- time_in_hours
name

names(name) <- mph
name

df1 <- data.frame(name, distance, mph)
df1
