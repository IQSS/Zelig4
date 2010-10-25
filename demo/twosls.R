#	Example 1

# 	Attach sample data and variable names.
#	year	year
#	C	consumption
#	P	corporate profits
#	P1	previous year corporate profits 
#	Wtot	total wage	
#	Wp	private wage bill
#	Wg	goverment wage bill
#	I	investment
#	K1	previous year capital stock
#	X	GNP
#	G	govermment spending
#	T	Taxes
#	X1	previous year GNP
# 	Tm	Year-1931

data(klein)

# Suppose that we want to estimate the following list of equations

#	C~Wtot + P + P1
#	I~P + P1 + K1
#	Wp~ X + X1 + Tm

# with the following instrumental variable
#	~ P1 + K1 + X1 + Tm + Wg + G

# Write the formula conform Zelig syntax and the instrumental variable (required for "2sls"):
formula <- list(mu1=C~Wtot + P + P1,
               mu2=I~P + P1 + K1,
               mu3=Wp~ X + X1 + Tm,
               inst= ~ P1 + K1 + X1 + Tm + Wg + G)

# Estimate model and present a summary:
user.prompt()
z.out<-zelig(formula=formula, model="twosls",data=klein)
user.prompt()
summary(z.out)

# Set explanatory variables to their default (mean/mode) values

user.prompt()
x.out <-setx(z.out,x=x.out)

# Simulate quantities of interests and present a summary

user.prompt()
s.out <-sim(z.out,x=x.out)
user.prompt()
summary(s.out)

# Plot

user.prompt()
plot(s.out)


