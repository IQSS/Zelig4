#
library(Zelig)

data(turnout)


z <- zelig(vote ~ age*educate + race, model = "logit", data = turnout)

x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))

s <- sim(z, x=x.low, x1=x.high, num=10)
