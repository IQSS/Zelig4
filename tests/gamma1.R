#
library(Zelig)

data(turnout)

z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)

x.out1 <- setx(z.out1, age = 36, race = "white")

s.out1 <- sim(z.out1, x = x.out1, num = 10, bootstrap = T)

summary(s.out1)
q()

summary(z.out1)
vcov(z.out1)
coef(z.out1)
x.out1
plot(s.out1)
