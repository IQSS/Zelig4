# Attach the data frame
data(sanction)

# Fit the statistical model

z.out <- zelig(num ~ target + coop, model = "poisson", data = sanction)

# Set explanatory variables (in this case non are explicitly set)

x.out <- setx(z.out)

# Simulate the quantities of interest

s.out <- sim(z.out, x = x.out)

# Summary of the statistical model

summary(z.out)

# Summary of the simulated quantities of interest

summary(s.out)

# Plot the simulated quantities of interest

plot(s.out)
