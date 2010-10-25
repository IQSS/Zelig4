##  Attaching the sample turnout dataset:
data(turnout)

#####  Example 1:  Simple Example 

##  Generating empirical estimates:
user.prompt()
z.out1 <- zelig(vote ~ race + educate, model = "probit", data = turnout)
##  Viewing the regression output:
user.prompt()
summary(z.out1)

##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  
user.prompt()
x.out1 <- setx(z.out1)

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
x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))

user.prompt()
s.out2 <- sim(z.out1, x = x.low, x1 = x.high)
user.prompt()
summary(s.out2)
user.prompt()
plot(s.out2)




























