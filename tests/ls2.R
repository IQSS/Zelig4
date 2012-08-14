#
library(Zelig)

data(macro)

macro$country <- as.factor(macro$country)

z.out2 <- zelig(
                unem ~ gdp + trade + capmob + country,
                model = "ls",
                data = macro
                )

x.US <- setx(z.out2, country = "United States")
x.Japan <- setx(z.out2, country = "Japan")

s.out2 <- sim(z.out2, x = x.US, x1 = x.Japan)

summary(z.out2)
vcov(z.out2)
coef(z.out2)
x.US
x.Japan
plot(s.out2)
