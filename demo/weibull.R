data(coalition)

##  Creating a censored object for the dependent variable using
##  Surv(duration, ciep12), where duration is the dependent variable
##  (number of days alive during the observation period), and ciep12
##  is the censoring indicator (coded 0 if alive and 1 if dead at the
##  end of the observation period), and regressing this censored
##  object using the selected explanatory variables:  
user.prompt()
z.out <- zelig(Surv(duration, ciep12) ~ invest + polar + numst2 + crisis,
               model = "weibull", data = coalition, robust = TRUE)
user.prompt()
##  Viewing the regression output.  Note that the Weibull model
##  differs from the exponential model in that the Weibull has an
##  optional scale parameter.  (The exponential is a special case of
##  the Weibull with scale set to 1.)
summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables).
user.prompt()
x.out <- setx(z.out)

##  Simulating draws from the posterior distribution.
user.prompt()
s.out <- sim(z.out, x = x.out)
user.prompt()
##  Viewing the simulated quantities of interest (in this case, the
##  expected value is the ceteris paribus predicted duration): 
summary(s.out)

##  Plotting the differences in the expected values for the Weibull
##  predictions:
user.prompt()
plot(s.out)




