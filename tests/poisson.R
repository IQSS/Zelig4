library(Zelig)

data(sanction)

z <- zelig(num ~ target + coop, model = "poisson", data = sanction)

x <- setx(z)

s <- sim(z, x = x, num = 10)
