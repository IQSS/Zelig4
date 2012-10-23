library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate + age, model = "logit", data = turnout)
x <- setx(z, educate = 6:7, age = 17)
s <- sim(z, x, num = 200)

summary(s)

plot(s)
