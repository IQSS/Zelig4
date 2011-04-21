#####  Example 1: Basic Example with First Differences  #####

# Attach sample data and variable names:  

data(macro)

# Estimate model and present a summary:

z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "ls", data = macro)

# Set explanatory variables to their default (mean/mode) values, with
# high (80th percentile) and low (20th percentile) values:

x.high<- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))

x.high
x.low


# Generate first differences for the effect of high versus low trade on
# GDP:

s.out1 <- sim(z.out1, x = x.high, x1 = x.low)

# Summary of fitted statistical model

summary(z.out1)

# Summary of simualted quantities of interest

summary(s.out1)

# Plot of simulated quantities of interest

plot(s.out1)

#####  Example 2:  Using Dummy Variables #####

# Estimate a model with a dummy variable for each year and country.  
# Note that you do not need to create dummy variables, as the program 
# will automatically parse the unique values in the selected variables 
# into dummy variables.

z.out2 <- zelig(unem ~ gdp + trade + capmob + as.factor(country), 
                model = "ls", data = macro)

# Set values for the explanatory variables, using the default mean/mode
# values, with country set to the United States and Japan, respectively:
x.US <- setx(z.out2, country = "United States")
x.Japan <- setx(z.out2, country = "Japan")



# Simulate quantities of interest:
s.out2 <- sim(z.out2, x = x.US, x1 = x.Japan)

# Summary of fitted statistical model

summary(z.out2)

# Summary of simulated quantities of interest

summary(s.out2)

# Plot differences:  
plot(s.out2)
