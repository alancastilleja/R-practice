library(gtools)
library(tidyverse)
options(digits = 3)    # report 3 significant digits
?permutations

# Question 1a
p <- permutations(8,3)
p
nrow(p)

# Question 1b
jamaica <- permutations(3,3)
nrow(jamaica)

# Question 1c
jamaica_three <- nrow(jamaica) / nrow(p)

# Question 1d
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
set.seed(1)
B <- 10000
finish <- replicate(B, {
  jamaica_win <- sample(runners, 3)
  all(jamaica_win == "Jamaica")
})
mean(finish)

# Question 2a and 2b and 2c
entree <- combinations(6,1)
n1 <- nrow(entree)

sides <- combinations(6,3)
n2 <- nrow(sides)
n2

drink <- combinations(3,1)
n3 <- nrow(drink)
n3

n1 * n2 * n3
?sample
?expand.grid

meal <- expand.grid(entree = entree, sides = sides, drink = drink)
meal # double counted, should use permutations if want to do that

# Question 2d 
compute_entree <- function(x) {
  combination_entree <- x * 3 * nrow(combinations(6,2))
  return(combination_entree)
}
combo_entree <- sapply((1:12), compute_entree)
data.frame(sides = 1:12, combos = combo_entree) %>%
  filter(combos > 365)

# Question 2e
compute_side <- function(y) {
  combination_side <- 6 * 3 * nrow(combinations(y,2))
  return(combination_side)
}
combo_sides <- sapply(2:12, compute_side)
data.frame(sides = 2:12, combo = combo_sides) %>% filter(combo_sides > 365)

