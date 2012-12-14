data(mid)
user.prompt()


## prior correction + bias correction 
z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid, model = "relogit", tau = 1042/303772)
user.prompt()

summary(z.out1)
user.prompt()

x.out1 <- setx(z.out1)
user.prompt()

s.out1 <- sim(z.out1, x = x.out1)
user.prompt()

summary(s.out1)
user.prompt()

plot(s.out1)

## weighting + bias correction + robust s.e.
z.out2 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid, model = "relogit", tau = 1042/303772,
                case.control = "weighting", robust = TRUE)
user.prompt()

summary(z.out2)
user.prompt()

x.out2 <- setx(z.out2)
user.prompt()

s.out2 <- sim(z.out2, x = x.out2)
user.prompt()
