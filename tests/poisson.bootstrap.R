#
library(Zelig)

data(sanction)

z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)

x.out <- setx(z.out)

s.boot <- sim(z.out, x = x.out, bootstrap = TRUE)

s.parametric <- sim(z.out, x = x.out, bootstrap = FALSE)


summary(s.boot)
summary(s.parametric)

q()

summary(z.out)
vcov(z.out)
coef(z.out)
x.out
plot(s.out)
