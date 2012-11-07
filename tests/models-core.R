library(Zelig)

data(coalition)
data(macro)
data(mid)
data(tobin)
data(turnout)
data(sanction)

# exp
# exp
# exp

z.out <- zelig(Surv(duration, ciep12) ~ invest + polar + numst2 + crisis, model = "exp", data = coalition[1:100,])

x.low<- setx(z.out, numst1 = 0)
x.high <- setx(z.out, numst2 = 1)

s.out <- sim(z.out, x = x.low, x1 = x.high, num = 10)

# gamma
# gamma
# gamma

z <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)

x.low <- setx(z, numst2 = 0)
x.high <- setx(z, numst2 = 1)

s <- sim(z, x = x.low, x1 = x.high, num = 10)

plot(s)

# logit
# logit
# logit

z <- zelig(vote ~ age*educate + race, model = "logit", data = turnout)

x.high <- setx(z, educate = quantile(turnout$educate, probs = 0.75))
x.low <- setx(z, educate = quantile(turnout$educate, probs = 0.25))

s <- sim(z, x = x.low, x1 = x.high, num = 10)

# ls
# ls
# ls

z <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)

x.high <- setx(z, trade = quantile(trade, 0.8))
x.low <- setx(z, trade = quantile(trade, 0.2))

s <- sim(z, x = x.high, x1 = x.low, num = 10)

# negbinom
# negbinom
# negbinom

z <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

x <- setx(z)

s <- sim(z, x = x, num = 10)

# normal
# normal
# normal

z <- zelig(unem ~ gdp + capmob + trade, model = "normal", data = macro)

x.high <- setx(z, trade = quantile(trade, 0.8))
x.low <- setx(z, trade = quantile(trade, 0.2))

s <- sim(z, x = x.high, x1 = x.low)

# poisson
# poisson
# poisson

z <- zelig(num ~ target + coop, model = "poisson", data = sanction)

x <- setx(z)

s <- sim(z, x = x, num = 10)

q()

# probit
# probit
# probit

z <- zelig(vote ~ race + educate, model = "probit", data = turnout)

x.low <- setx(z, educate = quantile(turnout$educate, probs = 0.75))
x.high <- setx(z, educate = quantile(turnout$educate, probs = 0.25))

s <- sim(z, x = x.low, x1 = x.high, num = 10)

# relogit
# relogit
# relogit

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

# tobit
# tobit
# tobit

z <- zelig(durable ~ age + quant, data = tobin, model = "tobit")
x <- setx(z)
s <- sim(z, x = x, num = 10)
