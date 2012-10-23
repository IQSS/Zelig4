library(Zelig)

data(turnout)

z <- zelig(vote ~ age*educate + race, model = "logit", data = turnout)

x.high <- setx(z, educate = quantile(turnout$educate, probs = 0.75))
x.low <- setx(z, educate = quantile(turnout$educate, probs = 0.25))

message("<<")
s <- sim(z, x = x.low, x1 = x.high, num = 10)
message(">>")

summary(s)
