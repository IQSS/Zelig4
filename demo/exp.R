data(coalition)

##  Creating a censored object for the dependent variable using
##  Surv(duration, ciep12), where duration is the dependent variable
##  (number of days alive during the observation period), and ciep12
##  is the censoring indicator (coded 0 if alive and 1 if dead at the
##  end of the observation period), and regressing this censored
##  object using the selected explanatory variables:  
user.prompt()
z.out <- zelig(Surv(duration, ciep12) ~ invest + polar + numst2 + crisis,
               model = "exp", data = coalition)
user.prompt()
summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
##  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
user.prompt()
x.low<- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

##  Simulating draws using the default bootstrap method.
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()

##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
user.prompt()
plot(s.out)


