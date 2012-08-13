library(Zelig)

data(turnout)

z <- zelig(vote ~ educate + race, model = "logit", data = turnout)
x <- setx(z, age=36, educate=4, race="white")
x1 <- setx(z, age=36, educate=40, race="white")
s <- sim(z, x, x1)

summary(s)
plot(s)
