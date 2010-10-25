data(turnout)
z.out <- zelig(vote ~ age + educate + income, by = "race",
               data = turnout, model = "probit")
user.prompt()
x.white <- setx(z.out$others, fn = NULL, data = turnout[turnout$race == "white",], cond = TRUE)
user.prompt()
s.others <- sim(z.out$others, x = x.white)
summary(s.others)
user.prompt()
x.others <- setx(z.out$white, fn = NULL, data = turnout[turnout$race == "others",], cond = TRUE)
s.others <- sim(z.out$white, x = x.others)
summary(s.others)

