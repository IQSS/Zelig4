#####  Example 1: Basic Example with First Differences  #####

# Attach sample data and variable names:  
data(macro)

# Estimate model and present a summary:
user.prompt()
z.out1 <- zelig(unem ~ gdp + capmob + trade, model = "normal", data = macro)
user.prompt()
summary(z.out1)

# Set explanatory variables to their default (mean/mode) values, with
# high (80th percentile) and low (20th percentile) values:
user.prompt()
x.high <- setx(z.out1, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out1, trade = quantile(macro$trade, 0.2))

# Generate first differences for the effect of high versus low trade on
# GDP:
user.prompt()
s.out1 <- sim(z.out1, x = x.high, x1 = x.low)
user.prompt()
summary(s.out1)

# Generate a second set of fitted values and a plot:
user.prompt()
plot(s.out1)

#####  Example 2:  Using Dummy Variables #####

# Estimate a model with a dummy variable for each year and country.  
# Note that you do not need to create dummy variables, as the program 
# will automatically parse the unique values in the selected variables 
# into dummy variables.  
#user.prompt()
#z.out2 <- zelig(unem ~ gdp + trade + capmob + as.factor(year) 
#                    + as.factor(country), model = "normal", data = macro)

# Set values for the explanatory variables, using the default mean/mode
# values, with country set to the United States and Japan, respectively:
#user.prompt()
#x.US <- setx(z.out2, country = "United States")
#x.Japan <- setx(z.out2, country = "Japan")

# Simulate quantities of interest:
#user.prompt()
#s.out2 <- sim(z.out2, x = x.US, x1 = x.Japan)
#user.prompt()
#summary(s.out2) 

# Plot differences:  
#user.prompt()
#plot(s.out2)


