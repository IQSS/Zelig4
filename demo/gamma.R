data(coalition)
user.prompt()
z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)
user.prompt()
summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
##  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
user.prompt()
x.low <- setx(z.out, numst2 = 0)
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




