# Tests for multiple/pooled data

library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)
x <- setx(z, educate = c(4, 15))
s <- sim(z, x)

pooled.sim <- s
sample.qi <- s[[1]]$qi

names.call <- call("names", as.name("sample.qi"))
extract.qi.call1 <- call("[[", as.name("sample.qi"), "Expected Values: E(Y|X)")
extract.qi.call2 <- call("$", as.name("sample.qi"), "ev1")

save(pooled.sim, sample.qi, names.call, extract.qi.call1, extract.qi.call2, file = "here.Rsave")


# Fin.
