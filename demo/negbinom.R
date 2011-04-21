library(Zelig)

data(sanction)


z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

summary(z.out)

x.out <- setx(z.out)
x.out

s.out <- sim(z.out, x = x.out)

plot(s.out)
