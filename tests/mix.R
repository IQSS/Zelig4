library(Zelig)

data(turnout)

z <- zelig(vote ~ race, model = "logit", data = turnout)
x <- setx(z, race = "others")
s <- sim(z, x)

summary(x)

# Fin.
