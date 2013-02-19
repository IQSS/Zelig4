library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)

x.single <- setx(z, educate = 5)
x.pooled <- setx(z, educate = 5:7)

s.single <- sim(z, x.single, num = 5)
s.pooled <- sim(z, x.pooled, num = 5)

as.matrix(x.single)
as.matrix(x.pooled)

# simulation.matrix(s.single, "Expected Values")
summary(s.pooled)
sm <- simulation.matrix(s.pooled, "Expected Values")
attr(sm, "which")
# simulation.matrix(s.pooled, "Predicted Values")


