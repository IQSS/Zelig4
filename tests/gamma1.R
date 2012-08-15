#
library(Zelig)

data(turnout)

z.out1 <- zelig(vote ~ age + race, model = "gamma", data = turnout)

x.out1 <- setx(z.out1, age = 36, race = "white")
x.out2 <- setx(z.out1, age = 20, educate = 4)

s.out2 <- sim(z.out1, x.out1, x.out2, num = 10, bootstrap = F)

summary(z.out1)
vcov(z.out1)
coef(z.out1)
x.out1
plot(s.out1)
