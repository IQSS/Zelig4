# Load the sample data:
data(sanction)

#####  Example 1: Basics #####

# Note that by default, zelig() estimates two parameter estimates
# for explanatory variable as well as the correlation parameter; this
# formulation is parametrically independent (estimating separate effects
# for each explanatory variable), but stochastically dependent because
# the models share an odds ratio.  See Example 2 for a more constrained
# form of stochastic dependence.a list of at most 3 elements (corresponding to the 3
# equations).  Each element consists of a character vector of the
# variables omitted from each equation.  
z.out1 <- zelig(cbind(import, export) ~ coop + cost + target,
                model = "blogit", data = sanction)
user.prompt()
print(summary(z.out1))
user.prompt()
# Generate baseline values for the explanatory variables (with cost set
# to 1, net gain to sender) and alternative values (with cost set to 4,
# major loss to sender):
x.low <- setx(z.out1, cost = 1)
x.high <- setx(z.out1, cost = 4)
# Simulate fitted values and first differences:  
user.prompt()
s.out1 <- sim(z.out1, x = x.low, x1 = x.high)
user.prompt()
print(summary(s.out1))

# Plot the s.out
user.prompt()
plot(s.out1)
user.prompt()

##### Example 2: Joint Estimation of a Model with        #####
#####            Different Sets of Explanatory Variables #####

# Estimate the statistical model, with import a function of coop
# in the first equation and export a function of cost and target
# in the second equation, by using the zeros argument:
z.out2 <- zelig(list(mu1=import~coop,mu2=export~cost+target), 
                model = "blogit", data = sanction)
user.prompt()
print(summary(z.out2))
user.prompt()
# Set the explanatory variables to their default values:
x.out2 <- setx(z.out2)

# Simulate draws from the posterior distribution:
user.prompt()
s.out2 <- sim(z.out2, x = x.out2)
user.prompt()
print(summary(s.out2))

# Plotting marginal densities:
user.prompt()
plot(s.out2)

##### Example 3: Joint Estimation of a Parametrically #####
##### and Stochastically Dependent Model              #####

# A bivariate model is parametrically dependent if Y1 and Y2 share
# some or all explanatory variables, {\it and} the effects of the shared
# explanatory variables are jointly estimated.  For example,
user.prompt()
z.out3 <- zelig(cbind(import, export) ~ coop + cost + target, 
                constrain = list("1" = c("coop", "cost", "target"),
                                 "2" = c("coop", "cost", "target")),
                model = "blogit", data = sanction)
user.prompt()
print(summary(z.out3))

# Note that this model only returns one parameter estimate for each of
# coop, cost, and target.  Contrast this to Example 1 which returns two
# parameter estimates for each of the explanatory variables.

# Set values for the explanatory variables:
user.prompt()
x.out3 <- setx(z.out3, cost = 1:4)

# Draw simulated expected values:  
user.prompt()
s.out3 <- sim(z.out3, x = x.out3)
user.prompt()
print(summary(s.out3))




