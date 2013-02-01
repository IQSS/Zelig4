library(Zelig)

data(mid)

z <- zelig(conflict ~ major + contig + power + maxdem + mindem + years, model = "relogit", tau = 1042/303772, data = mid)
x <- setx(z)
s <- sim(z, x)

summary(s)

plot(s)

## weighting + bias correction + robust s.e.
z <- zelig(conflict ~ major + contig + power + maxdem + mindem + years,
           data = mid, model = "relogit", tau = 1042/303772,
           case.control = "weighting", robust = TRUE)
x <- setx(z)
s <- sim(z, x)

summary(s)
