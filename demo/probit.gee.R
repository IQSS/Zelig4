##  Attaching the sample turnout dataset:
data(turnout)

##  Variable identifying clusters
turnout$cluster <- rep(c(1:200),10)

## Sorting by cluster
sorted.turnout <- turnout[order(turnout$cluster),]

#####  Example 1:  Simple Example with Stationary 3 Dependence

##  Generating empirical estimates:
user.prompt()
z.out1 <- zelig(vote ~ race + educate, model = "probit.gee", id = "cluster", 
	data = sorted.turnout, robust = T, corstr = "stat_M_dep", Mv=3)
user.prompt()
##  Viewing the regression output:
summary(z.out1)

##  Using setx to generate baseline and alternative values for the
##  explanatory variables.  
user.prompt()
x.out1 <- setx(z.out1)

##  Simulating quantities of interest:
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
x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))

user.prompt()
s.out2 <- sim(z.out1, x = x.high, x1 = x.low)
user.prompt()
summary(s.out2)
user.prompt()
plot(s.out2)

#####  Example 3:  Example with Fixed Correlation Structure

##  User-defined correlation structure
user.prompt()
corr.mat <- matrix(rep(0.5,100), nrow=10, ncol=10)
diag(corr.mat) <- 1 

##  Generating empirical estimates:
user.prompt()
z.out2 <- zelig(vote ~ race + educate, model = "probit.gee", id = "cluster", 
	data = sorted.turnout, robust = T, corstr = "fixed", R=corr.mat)
user.prompt()
##  Viewing the regression output:
summary(z.out2)

















































