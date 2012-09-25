library(Zelig)

data(mid)


z.out1 <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid, model = "relogit",
                tau = 1042/303772)

z.out2 <- zelig(
                conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid,
                model = "relogit",
                tau = 1042/303772,
                case.control = "weighting",
                robust = TRUE
                )

x.out1 <- setx(z.out1)
x.out2 <- setx(z.out2)

s.out1 <- sim(z.out1, x = x.out1, num=10)
s.out2 <- sim(z.out2, x = x.out2, num=10)
