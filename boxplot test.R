library(dslabs)
data("olive")
head(olive)
summary(olive)
str(olive)

plot(olive$palmitic, olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic ~ region, data = olive)
