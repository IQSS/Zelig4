library(Zelig)

data(coalition)

z.out <- zelig(duration ~ fract + numst2, model = "gamma", data = coalition)

summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
##  with numst2 set to the vector 0 = no crisis, 1 = crisis. 
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

x.low
x.high

##  Simulating draws using the default bootstrap method.

s.out <- sim(z.out, x = x.low, x1 = x.high)


##  Viewing the simulated quantities of interest, for every
##  observation:

plot(s.out)
