library(Zelig)

# mix(list('a'))
# mix(list('a', 'b', 'c'), list(1, 2, 3, 4))
#
#
#

data(turnout)

z1 <- zelig(vote ~ race, model = "logit", data = turnout)
x1 <- setx(z1, race = "others")
summary(x1)

z2 <- zelig(vote ~ race, model = "logit", data = turnout)
x2 <- setx(z1, race = c("white", "others"))
summary(x2)

z3 <- zelig(vote ~ race + educate, model = "logit", data = turnout)
x3 <- setx(z2, race = "others", educate = 10:15)
class(x3)
summary(x3)

z4 <- zelig(vote ~ race + educate, model = "logit", data = turnout)
x4 <- setx(z3)
summary(x4)

# Fin.
