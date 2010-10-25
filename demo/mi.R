data(immi1, immi2, immi3, immi4, immi5)
user.prompt()

z.out <- zelig(as.factor(ipip) ~ wage1992 + prtyid + ideol, model = "ologit",
               data = mi(immi1, immi2, immi3, immi4, immi5), by = "gender")
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
user.prompt()
z.out <- zelig(as.factor(ipip) ~ wage1992 + prtyid + ideol, model = "mlogit",
               data = mi(immi1, immi2, immi3, immi4, immi5))
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


