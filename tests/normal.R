library(Zelig)

data(macro)

z <- zelig(unem ~ gdp + capmob + trade, model = "normal", data = macro)

x.high <- setx(z, trade = quantile(trade, 0.8))
x.low <- setx(z, trade = quantile(trade, 0.2))

s <- sim(z, x = x.high, x1 = x.low)
