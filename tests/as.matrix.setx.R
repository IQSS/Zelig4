library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)

x.single <- setx(z, educate = 5)
x.pooled <- setx(z, educate = 5:7)

# as.matrix(x.single)
as.matrix(x.pooled)
