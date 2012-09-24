#
message("0 >>>>>>>>")
library(Zelig)

data(turnout)


message("1 >>>>>>>")
z <- zelig(vote ~ age*educate + race, model = "logit", data = turnout)

message("2 >>>>>>>")
x.high <- setx(z, educate = quantile(turnout$educate, probs = 0.75))
x.low <- setx(z, educate = quantile(turnout$educate, probs = 0.25))

message("3 >>>>>>>")
s <- sim(z, x=x.low, x1=x.high, num=10)

summary(s)
