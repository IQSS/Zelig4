#
library(Zelig)

data(sanction)

z <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

x <- setx(z)

s <- sim(z, x = x)

summary(z)
