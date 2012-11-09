#####  Example 1: Basic Example #####

# Attach sample data and variable names:  
data(coalition)

#  Variable identifying clusters
coalition$cluster <- c(rep(c(1:62),5),rep(c(63),4))

# Sorting by cluster
sorted.coalition <- coalition[order(coalition$cluster),]

# Estimate model and present a summary:
user.prompt()
z.out <- zelig(duration ~ fract + numst2, model = "gamma.gee", id = "cluster", data = sorted.coalition, robust=TRUE, corstr="exchangeable")
user.prompt()
summary(z.out)

#  Setting the explanatory variables at their default values
#  (mode for factor variables and mean for non-factor variables),
#  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
user.prompt()
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)


# Simulate quantities of interest
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()
summary(s.out)

# Generate a plot of quantities of interest:
user.prompt()
plot(s.out)









