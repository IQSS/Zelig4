library(Amelia)
library(Zelig)

data(turnout)

z <- zelig(vote ~ educate + income, model = "logit", by = "race", data = turnout)
x <- setx(z, educate = 4)
s <- sim(z, x)
summary(s)
