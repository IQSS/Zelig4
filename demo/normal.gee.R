#####  Example 1: Basic Example with First Differences  #####

# Attach sample data and variable names:  
data(macro)

# Estimate model and present a summary:
user.prompt()
z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.gee", id = "country", data = macro, robust=TRUE, corstr="AR-M", Mv=1)
user.prompt()
summary(z.out)

# Set explanatory variables to their default (mean/mode) values, with
# high (80th percentile) and low (20th percentile) values:
user.prompt()
x.high <- setx(z.out, trade = quantile(macro$trade, 0.8))
x.low <- setx(z.out, trade = quantile(macro$trade, 0.2))

# Generate first differences for the effect of high versus low trade on
# GDP:
user.prompt()
s.out <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()
summary(s.out)

# Generate a plot of quantities of interest:
user.prompt()
plot(s.out)









