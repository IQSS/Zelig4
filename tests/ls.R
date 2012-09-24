#
library(Zelig)

data(macro)

z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)

x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))

s.out1 <- sim(z.out2, x = x.high, x1 = x.low)
