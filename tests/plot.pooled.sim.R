library(Zelig)

data(turnout)

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=3:24)
s <- sim(z, x)

plot(s)
