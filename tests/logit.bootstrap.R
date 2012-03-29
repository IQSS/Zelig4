#
library(Zelig)

data(turnout)

z.out1 <- zelig(
                vote ~ sin(educate) + age*educate,
                model = "logit",
                data = turnout
                )

x.out1 <- setx(z.out1, age = 36, race = "white", educate=0)

s.out1 <- sim(z.out1, x = x.out1, num = 20, bootstrap = TRUE)

summary(s.out1)
plot(s.out1)
