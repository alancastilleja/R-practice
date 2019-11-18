# Question 1 + 2

# Question 1, propability that ball will be cyan
balls <- rep( c("cyan", "magenta", "yellow"), times = c(3, 5, 7))
balls
tab_balls <- table(balls)
tab_balls
prop.table(tab_balls)
prob_cyan <- mean(balls == "cyan")
prob_notcyan <- mean(balls != "cyan")

# Question 2, propability not cyan
tab_balls
prop.table(tab_balls)
mean(balls!="cyan")

# Question 3, without replacement
balls_noreplace <- rep( c("cyan", "magenta", "yellow"), times = c(2, 5, 7))
tab_ballsnoreplace <- table(balls_noreplace)
prop.table(tab_ballsnoreplace)
prob_notcyan_noreplace <- mean(balls_noreplace!="cyan")
a <- prob_cyan * prob_notcyan_noreplace
options(digits = 3)
a

# Question 4, with replacement
p <- prob_cyan * prob_notcyan
options(digits = 3)
p
