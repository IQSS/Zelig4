library(Zelig)

data(turnout)

par(mfrow=c(2, 2))

z <- zelig(vote ~ income + educate, model="relogit", data=turnout)
x <- setx(z, educate=2:8)
x1 <- setx(z, educate=2:8, income = 10)
s <- sim(z, x, x1)

plot.ci(s, var="educate")

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=-5:5)
s <- sim(z, x)

plot.ci(s, var="educate", ylim = c(-2, 1))

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=-5:5)
s <- sim(z, x)

plot.ci(s, var="educate")

z <- zelig(vote ~ income + educate, model="logit", data=turnout)
x <- setx(z, educate=12)
s <- sim(z, x)

plot.ci(s)
