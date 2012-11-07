# Attach the data-frame
data(sanction)

# Fit the statistical model

z <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

# Set explanatory variables (in this case, nothing is explicitly set)

x <- setx(z)

# Simulate Quantities of Interest

s <- sim(z, x)

# Summarize the statistical model

summary(z)

# Summarize the simulated quantities of interest

summary (s)

# Plot the results

plot(s)
