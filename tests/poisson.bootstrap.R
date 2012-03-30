#
library(Zelig)

data(sanction)

z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)

x.out <- setx(z.out)

s.boot <- sim(z.out, x = x.out, num = 20, bootstrap = TRUE)

summary(s.boot)
