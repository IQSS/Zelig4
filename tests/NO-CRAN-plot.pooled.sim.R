library(Zelig)

data(turnout)

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=3:24)
s <- sim(z, x)

par(mfrow = c(2,1))

Zelig:::plot.ci(s)
plot(s)
