library(Zelig)

data(mid)

z.out2 <- zelig(
                conflict ~ major + contig + power + maxdem + mindem + years,
                data = mid,
                model = "relogit",
                tau = 1042/303772,
                case.control = "weighting",
                robust = TRUE
                )

summary(z.out2)

x.out2 <- setx(z.out2)

s.out2 <- sim(z.out2, x = x.out2)

summary(s.out2)
