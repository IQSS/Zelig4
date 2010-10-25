##### Example 1: Basic Examle with First Differences #####

# Sample data about the efficiency of a plant
data(stackloss)

#Estimate the model, observe the output
user.prompt()
z.out1 <- zelig(stack.loss ~  Air.Flow + Water.Temp + Acid.Conc., model = "quantile", 
                data = stackloss, tau=0.5)
summary(z.out1)

#Set explanatory variables
user.prompt()
x.high <- setx(z.out1, Water.Temp = quantile(stackloss$Water.Temp, 0.8))
x.low <- setx(z.out1, Water.Temp = quantile(stackloss$Water.Temp, 0.2))

#Simulate
 user.prompt()
 s.out1 <- sim(z.out1, x = x.high, x1 = x.low)
 summary(s.out1)
 plot(s.out1)

##### Example 2: Example Using Dummy Variables #####

#Macroeconomic data to demonstrate use of dummy variables.
#Note that to measure country-level effects, we only need to include
#the country factor variable, from which R will create a matrix of
#dummy variables. Convenient!
 user.prompt()
 data(macro)
 z.out2 <- zelig(unem ~ gdp + trade + capmob + as.factor(country), 
                  model = "quantile", tau=0.5, data = macro)

#Set values of the country dummy variable to explore first differences
#between the US and Japan holding other variables at their means
 user.prompt()
 x.US <- setx(z.out2, country = "United States")
 x.Japan <- setx(z.out2, country = "Japan")

#Simulate quantities of interest:
 user.prompt()
 s.out2 <- sim(z.out2, x = x.US, x1 = x.Japan)

#Plot results
 user.prompt()
 plot(s.out2)

##### Example 3: Example of Fitting Multiple Quantiles #####

#We estimate a model of food expenditure as a function of household income.
#This dataset is interesting because there is clear heteroskedasticity. Thus,
#estimating multiple quantiles lets us get a fuller picture of the conditional
#distribution of the data than a mean estimate from OLS would.
 user.prompt()
 data(engel)
 z.out3 <- zelig(foodexp ~ income, model = "quantile", tau=seq(0.1,0.9,by=0.1), data = engel)

#The summary function provides information about each fit that was specified
#in the call.
 user.prompt()
 summary(z.out3)

#We can also plot the coefficients of each fit and compare them to the OLS fit.
#This functionality is built into the quantile fitting routine and does not
#require the user to run the fit object through Zelig's simulation utilities.
 user.prompt()
 plot(summary(z.out3))

#Using setx, we can specify the levels of covariates as before.
 user.prompt()
 x.bottom <- setx(z.out3, income=quantile(engel$income, 0.25))
 x.top <- setx(z.out3, income=quantile(engel$income, 0.75))

#We run simulations without our counterfactual values. The simulation reruns
#every fit that was specified in zelig().
 user.prompt()
 s.out3 <- sim(z.out3, x = x.bottom, x1 = x.top)

#Summarize the results of all of the fits at once, or plot the results one as a time.
 user.prompt()
 summary(s.out3)
 user.prompt()
 plot(s.out3[[1]]) # You can plot any one of the sim outputs from 1 to 9
