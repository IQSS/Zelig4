##  Attaching the sample turnout dataset:
data(turnout)

#####  Example 1:  Simple Example 

##  Generating empirical estimates:
user.prompt()
z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)
user.prompt()
##  Viewing the regression output:
summary(z.out1)

##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  
user.prompt()
x.out1 <- setx(z.out1, age = 36, race = "white")

##  Simulating quantities of interest (predicted probabilites, risk
##  ratios, and risk differences):
user.prompt()
s.out1 <- sim(z.out1, x = x.out1)
user.prompt()
## Summarizing the simulated quantities of interest:
summary(s.out1)

## Diagnostic plot of the s.out:
user.prompt()
plot(s.out1)

##  Example 2: First Differences

user.prompt()
z.out2 <-  zelig(vote ~ race + educate, model = "logit", data = turnout)
x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))

user.prompt()
s.out2 <- sim(z.out2, x = x.high, x1 = x.low)
user.prompt()
summary(s.out2)
user.prompt()
plot(s.out2)




























