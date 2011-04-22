#
library(Zelig)

data(turnout)

z.out1 <- zelig(vote ~ race + educate, model = "probit", data = turnout)

x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))

s.out2 <- sim(z.out1, x = x.low, x1 = x.high)

summary(z.out1)
vcov(z.out1)
coef(z.out1)
x.low
x.high
plot(s.out2)
