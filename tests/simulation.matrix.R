library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)

x.single <- setx(z, educate = 5)
x.pooled <- setx(z, educate = 5:7)

# Small number of simulations for visibility
s.single <- sim(z, x.single, num = 5)
s.pooled <- sim(z, x.pooled, num = 5)

as.matrix(x.single)
as.matrix(x.pooled)

simulation.matrix(s.single, "Expected Values")
simulation.matrix(s.pooled, "Expected Values")
simulation.matrix(s.pooled, "Predicted Values")
