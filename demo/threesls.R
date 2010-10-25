# Example 1


# Attach sample data and variable names (Kmenta's simple supply/demand model). 
# 	q 	food consumption per capita.
#	p 	ratio of food prices to general consumer prices.
# 	d 	disposable income in constant dollars.
#     f 	ratio of preceding year's prices received by farmers to general consumer prices.
#	a 	time in years.


data(kmenta)

# Suppose that we want to estimate the following list of equations

#	q ~ p + d
#	q ~ p + f + a


# with the following instrumental variable
#	inst <- ~ d + f + a


# Write the formula according to Zelig syntax

formula<-list(	mu1= q ~ p + d,
		mu2=q ~ p + f + a,
		inst =~ d + f + a)

# estimate model and present a summary
user.prompt()
z.out<-zelig(formula=formula, model ="threesls",data=kmenta)
user.prompt()
summary(z.out)

# Set explanatory variables to their default (mean/mode) values
user.prompt()
x.out<-setx(z.out)

# Simulate the quantities of interest and present a summary

user.prompt()
s.out<-sim(z.out,x=x.out)
user.prompt()
summary(s.out)

# plot the quantities of interest for each equation

user.prompt()
plot(s.out)



