#
library(Zelig)

data(sanction)

z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(z.out)
vcov(z.out)
coef(z.out)
x.out
plot(s.out)
