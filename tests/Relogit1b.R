library(Zelig)

data(mid)

z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid, model = "relogit",
                tau = c(1042/303772, .2))

x.out1 <- setx(z.out1)

s.out1 <- sim(z.out1, x = x.out1)

q()


summary(s.out1)

plot(s.out1)
