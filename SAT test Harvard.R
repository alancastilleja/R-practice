options(digits = 3)
# Question 1a
prob <- 1/5
p_new <- 1/4
not_prob <- 1 - prob
not_p_new <- 1 - p_new
a <- 1
b <- -.25
b_new <- 0
q <- 44
# Question 1b
mu <-a*p + b*(1-p)

# Question 1c
q * mu

# Quesiton 1d
sigma <- sqrt(q) * abs(b - a) * sqrt(p * not_p)

# Question 1e
rm(se)

1 - pnorm(8, mean = mu, sd = sigma)

# Question 1f
set.seed(21)

SAT <- replicate(B , {
  test <- sample(c(1, -0.25), q, replace = TRUE, prob = c(p,not_p))
  results <- sum(test)
})
mean(SAT >= 8)

#Question 2a
mu_new <- 44 * a*p_new + b_new*(1 - p_new)
mu_new

# Question 2b
sigma_new <- sqrt(q) * abs(b_new - a) * sqrt(p_new * not_p_new)

format(1 - pnorm(30, mean = mu_new, sd = sigma_new), scientific = TRUE)

# Question 2c
z <-  seq(.25, .95, .05)
length(z)
z

x <- function(z) {
  sample(c(1,0), q, replace = TRUE, prob = c(z, 1-z))
  avg <- q * a*z + b_new*(1 - z)
  se <- sqrt(q) * abs(b_new - a) * sqrt(z * (1-z))
  1 - pnorm(35, avg, se)
  which.min(p >= .8)
}
sapply(p, x)

