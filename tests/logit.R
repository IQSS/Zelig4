library(Zelig)

##  Attaching the sample turnout dataset:
data(turnout)

#####  Example 1:  Simple Example 

##  Generating empirical estimates:
z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)


##  Viewing the regression output:
summary(z.out1)


##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  

x.out1 <- setx(z.out1, age = 36, race = "white")
x.out1
##  Simulating quantities of interest (predicted probabilites, risk
##  ratios, and risk differences):

s.out1 <- sim(z.out1, x = x.out1)

## Summarizing the simulated quantities of interest:
summary(s.out1)

## Diagnostic plot of the s.out:

plot(s.out1)

##  Example 2: First Differences

z.out2 <-  zelig(vote ~ race + educate, model = "logit", data =
                 turnout)

summary(z.out2)


x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))
x.high
x.low

s.out2 <- sim(z.out2, x = x.high, x1 = x.low)

plot(s.out2)




























