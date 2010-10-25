#### Example 1: Basic Example ####

data(coalition)

# Running the basic coxph model with robust clustered standard errors.
user.prompt()
z.out1 <- zelig(Surv(duration, ciep12) ~ invest + numst2 + crisis, 
               robust = TRUE, cluster = "polar", model = "coxph", 
               data = coalition)
user.prompt()
summary(z.out1)

#  Setting the explanatory variables at their default values
#  (mode for factor variables and mean for non-factor variables),
#  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
user.prompt()
x.low1<- setx(z.out1, numst2 = 0)
x.high1 <- setx(z.out1, numst2 = 1)

#  Simulating draws using the default method.
user.prompt()
s.out1 <- sim(z.out1, x = x.low1, x1 = x.high1)
user.prompt()

#  Viewing the simulated quantities of interest.
summary(s.out1)
user.prompt()
plot(s.out1)

#### Example 2: Example with Stratified Cox Model ####

# Running the stratified coxph model with strata defined by polar variable.
user.prompt()
z.out2 <- zelig(Surv(duration, ciep12) ~ invest + numst2 + crisis + strata(polar), 
                model = "coxph", data = coalition)
user.prompt()
summary(z.out2)

#  Setting the explanatory variables at their default values
#  with numst2 set to the vector 0 = no crisis, 1 = crisis and 
#  strata set to polar=3. 
user.prompt()
x.low2<- setx(z.out2, numst2 = 0, strata = "polar=3")
x.high2 <- setx(z.out2, numst2 = 1, strata = "polar=3")

#  Simulating draws using the default method.
user.prompt()
s.out2 <- sim(z.out2, x = x.low2, x1 = x.high2)
user.prompt()

#  Viewing the simulated quantities of interest.
summary(s.out2)
user.prompt()
plot(s.out2)

#### Example 3: Example with Time-Varying Covariates

#  Create sample toy dataset (from survival package):
user.prompt()
toy <- as.data.frame(list(start=c(1, 2, 5, 2, 1, 7, 3, 4, 8, 8),
            stop=c(2, 3, 6, 7, 8, 9, 9, 9,14,17),
            event=c(1, 1, 1, 1, 1, 1, 1, 0, 0, 0),
            x=c(1, 0, 0, 1, 0, 1, 1, 1, 0, 0),
	    x1=c(5, 5, 7, 4, 5, 6, 3, 2, 7, 4) ))

#  Estimating parameter values for the coxph regression:
user.prompt()
z.out3 <- zelig(Surv(start, stop, event) ~ x + x1, model = "coxph", data = toy)
user.prompt()
summary(z.out3)

#  Setting values for the explanatory variables:
user.prompt()
x.low3 <- setx(z.out3, x = 0)
x.high3 <- setx(z.out3, x = 1)

#  Simulating quantities of interest:
user.prompt()
s.out3 <- sim(z.out3, x = x.low3, x1 = x.high3)
user.prompt()

#  Viewing the simulated quantites of interest
summary(s.out3)
user.prompt()
plot(s.out3)





