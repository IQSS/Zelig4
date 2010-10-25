#### Example 1: No External Regressors ####
data(approval)

# Estimate the ARIMA model, and summarize the results

z.out1<- zelig(Diff(approve, 1)~lag.eps(2) + lag.y(2), data=approval, model="arima")
summary(z.out1)
user.prompt()

# Set the number of time periods ahead for the prediction to run


x.out1<- setx(z.out1, pred.ahead=10)



# Simulate the predicted quantities of interest
user.prompt()

s.out1<- sim(z.out1, x=x.out1)

# summarize and plot the results
user.prompt()

summary(z.out1)
plot(s.out1, lty.set=2)

#### Example 2: External Regressors, 1 Counterfactual, 1 Time Period ####

# Estimates an ARIMA model where we include exogenous regressors 
# in addition to lagged errors and lagged values of the dependent variable.  
# This example shows the output if only one counterfactual value is specified for
# a time period.  

z.out2<- zelig(Diff(approve, 1)~ iraq.war + sept.oct.2001 + avg.price + lag.eps(1) + lag.y(2),
               data=approval, model="arima")

# Set the both the value and time period of counterfactual of interest.
user.prompt()
x.out2<- setx(z.out2, sept.oct.2001=list(time=45, value=0), cond=TRUE)

# Simulate the quantities of interest
user.prompt()

s.out2<-sim(z.out2, x=x.out2) 

# Summarizing and plotting the quantities of interest

user.prompt()

summary(s.out2)
plot(s.out2)



#### Example 3: External Regressors, Counterfactuals Over Many Time Periods ####

# This example continues to use the same model specification as above, but will show
# the output when several counterfactual values are specified.  

user.prompt()

x.out3<- setx(z.out2, sept.oct.2001=list(time=45:50, value=0))
x1.out3<- setx(z.out2, sept.oct.2001=list(time=45:50, value=1))

# Simulating the quantities of interest

user.prompt()

s.out3<- sim(z.out2, x=x.out3, x1=x1.out3)
# Summarizing and plotting the quantities of interest.  Here we are 
# only displaying the uncertainty resulting from parameter estimation  

summary(s.out3)
plot(s.out3, pred.se=FALSE)
