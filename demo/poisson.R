data(sanction)
user.prompt()

z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)
user.prompt()
summary(z.out)
user.prompt()

x.out <- setx(z.out)
user.prompt()

s.out <- sim(z.out, x = x.out)
user.prompt()
summary(s.out)
user.prompt()
plot(s.out)
