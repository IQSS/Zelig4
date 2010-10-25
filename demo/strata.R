
data(turnout)
z.out1 <- zelig(vote ~ educate + age + income, model = "logit", data = turnout, by = "race")
user.prompt()
##  Viewing the regression output:
summary(z.out1)

##  Using setx to generate baseline and alternative values for the
##  explanatory variables.  
user.prompt()
x.out1 <- setx(z.out1, age = 65)

##  Simulating quantities of interest (predicted probabilites, risk
##  ratios, and risk differences):
user.prompt()
s.out1 <- sim(z.out1, x = x.out1)
user.prompt()
## Summarizing the simulated quantities of interest:
summary(s.out1)
user.prompt()

## Conditional prediction:
x.out2 <- setx(z.out1, fn = NULL, cond = TRUE)
s.out2 <- sim(z.out1, x = x.out2)
user.prompt()
summary(s.out2)

