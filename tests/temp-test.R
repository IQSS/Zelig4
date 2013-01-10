library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit.bayes", data = turnout)
# z <- zelig(vote ~ race + educate, model = "logit.bayes", data = list(fuck=turnout, turnout, dick=turnout))
x <- setx(z)
s <- sim(z, x)

summary(s)


# Fin.
