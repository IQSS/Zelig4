#####  Example 1: Basic Example #####

# Attach sample data and variable names:  
data(sanction)

#  Variable identifying clusters
sanction$cluster <- c(rep(c(1:15),5),rep(c(16),3))

# Sorting by cluster
sorted.sanction <- sanction[order(sanction$cluster),]

# Estimate model and present a summary:
user.prompt()
z.out <- zelig(num ~ target + coop, model = "poisson.gee", id = "cluster", data = sorted.sanction, robust=TRUE, corstr="exchangeable")
user.prompt()
summary(z.out)

# Set explanatory variables to their default values:
user.prompt()
x.out <- setx(z.out)

# Simulate quantities of interest
user.prompt()
s.out <- sim(z.out, x = x.out)
user.prompt()
summary(s.out)

# Generate a plot of quantities of interest:
user.prompt()
plot(s.out)









