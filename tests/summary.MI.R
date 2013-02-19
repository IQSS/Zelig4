library(Zelig)

data(turnout)

d1 <- turnout[1:500, ]
d2 <- turnout[501:1000, ]
d3 <- turnout[1001:2000, ]

z <- zelig(vote ~ I(educate*income) + educate, model = "logit", data = mi(d1, d2, d3))

summary(z, subset = c(1, 3))

# F
