library(Zelig)

data(turnout)

z <- zelig(vote ~ age, model = "logit", data = mi(turnout[1:10, ], turnout[100:110, ]))

x <- setx(z, age = 90)

s <- sim(z, x=x, num=20)
