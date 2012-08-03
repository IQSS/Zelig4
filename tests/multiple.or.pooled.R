# Tests for multiple/pooled data

library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)
x <- setx(z, educate = c(4, 15))
s <- sim(z, x, num=1)

pooled.sim <- s
sample.qi <- s[[1]]$qi

# Fin.
