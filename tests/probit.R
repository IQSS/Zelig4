library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "probit", data = turnout)

x.low <- setx(z, educate = quantile(turnout$educate, probs = 0.75))
x.high <- setx(z, educate = quantile(turnout$educate, probs = 0.25))

s <- sim(z, x = x.low, x1 = x.high, num = 10)
