data(homerun)
user.prompt()
z.out <- zelig(homeruns ~ player + tag(player - 1 | month),
                   data=homerun, model="poisson.mixed")
user.prompt()
summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
user.prompt()
x.out <- setx(z.out)

##  Simulating draws using the default bootstrap method.
user.prompt()
s.out <- sim(z.out, x = x.out)
user.prompt()

##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
user.prompt()
