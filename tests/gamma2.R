#
library(Zelig)

data(turnout)

z.out2 <-  zelig(
                 vote ~ race + educate,
                 model = "logit",
                 data = turnout
                 )

summary(z.out2)

x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))

s.out2 <- sim(z.out2, x = x.high, x1 = x.low)

summary(z.out2)
vcov(z.out2)
coef(z.out2)
x.high
x.low
plot(s.out2)
