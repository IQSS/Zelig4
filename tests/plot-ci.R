library(Zelig)

data(turnout)

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=3:24)
s <- sim(z, x)

plot.ci(s, var="educate")

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=3)
s <- sim(z, x)

plot.ci(s, var="educate")
