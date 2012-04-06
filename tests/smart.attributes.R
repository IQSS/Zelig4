library(Zelig)

data(turnout)

z <- zelig(vote ~ log(1+income) + age*educate, model = "logit", data = turnout)

x.young <- setx(z, age = min(age))
x.old <- setx(z, age = max(age))
x.sum <- setx(z, age = sum(age))

x.interact <- setx(z)

# x.young
x.interact$matrix
