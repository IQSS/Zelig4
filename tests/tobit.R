library(Zelig)

data(tobin)

z <- zelig(durable ~ age + quant, data = tobin, model = "tobit")

x <- setx(z)

s <- sim(z, x = x, num = 10)
