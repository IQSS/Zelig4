library(Zelig)

data(turnout)

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=2:23)
s <- sim(z, x)

plot.ci(s, var="educate")

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=12)
s <- sim(z, x)

plot.ci(s, var="educate")

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=12)
s <- sim(z, x)

plot.ci(s)
