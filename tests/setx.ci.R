library(Zelig)

data(turnout)

z <- zelig(vote ~ race + educate, model = "logit", data = turnout)

x <- setx(z, educate = c(4, 15))
x <- setx(z, educate = 4)

s <- sim(z, x)

for (key in names(s)) {

  print(key)
  print(s[[key]])
  q()
  message()
  message()
}



# Fin.
