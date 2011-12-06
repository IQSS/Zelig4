library(Zelig)

data(turnout)

z1 <- zelig(vote ~ race + income, model="logit", data=turnout, cite=F)
z2 <- zelig(vote ~ race + sin(income), model="logit", data=turnout, cite=F)
z3 <- zelig(vote ~ race + income:educate, model="logit", data=turnout, cite=F)

t(setx(z1))
t(setx(z2))
t(setx(z3))
