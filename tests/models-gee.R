library(Zelig)

data(coalition)
data(turnout)
data(macro)
data(sanction)


cluster <- c(rep(c(1:62),5), rep(c(63),4))
coalition$cluster <- cluster

z.out <- zelig(duration ~ fract + numst2, 
               id = "cluster",
               model = "gamma.gee",
               data = coalition,
               corstr="exchangeable"
               )

summary(z.out)

#  Setting the explanatory variables at their default values
#  (mode for factor variables and mean for non-factor variables),
#  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

# Simulate quantities of interest
s.out <- sim(z.out, x = x.low, x1 = x.high)

summary(s.out)

# Generate a plot of quantities of interest:
plot(s.out)









##  Attaching the sample turnout dataset:

turnout$cluster <- rep(c(1:200),10)
sorted.turnout <- turnout[order(turnout$cluster),]

z.out1 <- zelig(
                vote ~ race + educate, model = "logit.gee",
                id = "cluster", 
	        data = turnout,
                corstr = "stat_M_dep",
                Mv=3
                )
summary(z.out1)

x.out1 <- setx(z.out1)
s.out1 <- sim(z.out1, x = x.out1)

summary(s.out1)
plot(s.out1)


x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))
s.out2 <- sim(z.out1, x = x.high, x1 = x.low)

summary(s.out2)
plot(s.out2)

q()
#####  Example 3:  Example with Fixed Correlation Structure

##  User-defined correlation structure
corr.mat <- matrix(rep(0.5,100), nrow=10, ncol=10)
diag(corr.mat) <- 1 

##  Generating empirical estimates:
z.out2 <- zelig(vote ~ race + educate, model = "logit.gee", id = "cluster", 
	data = sorted.turnout, robust = T, corstr = "fixed", R=corr.mat)
##  Viewing the regression output:
summary(z.out2)

















































z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.gee", id = "country", data = macro, robust=TRUE, corstr="AR-M", Mv=1)
summary(z.out)

# Set explanatory variables to their default (mean/mode) values, with
# high (80th percentile) and low (20th percentile) values:
x.high <- setx(z.out, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out, trade = quantile(macro$trade, 0.2))

# Generate first differences for the effect of high versus low trade on
# GDP:
s.out <- sim(z.out, x = x.high, x1 = x.low)
summary(s.out)

# Generate a plot of quantities of interest:
plot(s.out)

#
#
#

sanction$cluster <- c(rep(c(1:15),5),rep(c(16),3))

z.out <- zelig(num ~ target + coop, model = "poisson.gee", id = "cluster", data = sanction, robust=TRUE, corstr="exchangeable")
summary(z.out)

x.out <- setx(z.out)
s.out <- sim(z.out, x = x.out)

summary(s.out)
plot(s.out)









turnout$cluster <- rep(c(1:200),10)

z.out1 <- zelig(vote ~ race + educate, model = "probit.gee", id = "cluster", 
	data = turnout, robust = T, corstr = "stat_M_dep", Mv=3)
summary(z.out1)

x.out1 <- setx(z.out1)
s.out1 <- sim(z.out1, x = x.out1)

plot(s.out1)

x.high <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out1, educate = quantile(turnout$educate, prob = 0.25))

s.out2 <- sim(z.out1, x = x.high, x1 = x.low)

summary(s.out2)

plot(s.out2)

#####  Example 3:  Example with Fixed Correlation Structure

##  User-defined correlation structure
corr.mat <- matrix(rep(0.5,100), nrow=10, ncol=10)
diag(corr.mat) <- 1 

##  Generating empirical estimates:
z.out2 <- zelig(vote ~ race + educate, model = "probit.gee", id = "cluster", 
	data = turnout, robust = T, corstr = "fixed", R=corr.mat)

##  Viewing the regression output:
summary(z.out2)
