library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model="logit", data=turnout)
x <- setx(z, educate=10:14)
s <- sim(z, x)

class(s)

summary(s)


