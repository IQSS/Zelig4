##  Attaching the sample turnout dataset:
data(turnout)

#####  Example 1:  Simple Example 

##  Generating empirical estimates:

z.out1 <- zelig(vote ~ race + educate, model = "probit", data = turnout)
##  Viewing the regression output:


##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  

x.out1 <- setx(z.out1)
x.out1


##  Simulating quantities of interest (predicted probabilites, risk
##  ratios, and risk differences):

s.out1 <- sim(z.out1, x = x.out1)

# Summary of fitted the statistical model

summary(z.out1)

# Summary of the simulated quantities of interest

summary(s.out1)

## Diagnostic plot of the s.out:

plot(s.out1)

##  Example 2: First Differences

x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))

# Simulate quantities of interest (include first-differences, etc.)

s.out2 <- sim(z.out1, x = x.low, x1 = x.high)

# Summary of quantities of interest (for difference in x.low and x.high

summary(s.out2)

# Plot of quantities of interest

plot(s.out2)
