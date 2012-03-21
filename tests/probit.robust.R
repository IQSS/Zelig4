#
library(Zelig)

data(turnout)

z.out1 <- zelig(vote ~ race + educate, robust = TRUE, model = "probit",
                data = turnout)

x.out1 <- setx(z.out1)

s.out1 <- sim(z.out1, x = x.out1)

summary(z.out1)
coef(z.out1)
vcov(z.out1)
x.out1
plot(s.out1)
