# Attach the data frame
data(turnout)

##  Generating empirical estimates:

z.out1 <- zelig(vote ~ age + race, model = "logit", data = turnout)

##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  

x.out1 <- setx(z.out1, age = 36, race = "white")
x.out1

##  Simulating quantities of interest (predicted probabilites, risk
##  ratios, and risk differences):

s.out1 <- sim(z.out1, x = x.out1)

# Summary of fitted statistical model

summary(z.out1)

# Summary of simulations of quantities of interest

summary(s.out1)

# Plot simulations of quantities of interest

plot(s.out1)

##  Example 2: First Differences

# Fit the statistical model

z.out2 <-  zelig(vote ~ race + educate, model = "logit", data = turnout)

# Set alternate values

x.high <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out2, educate = quantile(turnout$educate, prob = 0.25))

s.out2 <- sim(z.out2, x = x.high, x1 = x.low)

# Summary of the fitted model

summary(z.out2)

# Summary of the simulated quantities of interest

summary(s.out2)

# Plot of the simulated quantities of interest

plot(s.out2)
