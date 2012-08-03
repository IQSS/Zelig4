library(Zelig)

data(mid)


z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid, model = "relogit",
                tau = 1042/303772)
                #tau = c(1042/303772, .2))

summary(z.out1)

message("!")
x.out1 <- setx(z.out1)
q()

s.out1 <- sim(z.out1, x = x.out1)


summary(s.out1)

plot(s.out1)
