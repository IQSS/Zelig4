# Attach the data-frame
data(sanction)

# Fit the statistical model

z.out <- zelig(num ~ target + coop, model = "negbinom", data = sanction)

# Set explanatory variables (in this case, nothing is explicitly set)

x.out <- setx(z.out)

# Simulate Quantities of Interest

s.out <- sim(z.out, x = x.out)

# Summarize the statistical model

summary(z.out)

# Summarize the simulated quantities of interest

summary (s,out)

# Plot the results

plot(s.out)
