#
library(Zelig)

data(sanction)

z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(z.out)
x.out
plot(s.out)
