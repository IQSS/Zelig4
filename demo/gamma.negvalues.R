data(coalition)
user.prompt()
z.out <- zelig(duration ~ fract + numst2 + crisis, model = "gamma", data = coalition)
user.prompt()
summary(z.out)

##  Setting the explanatory variables.
user.prompt()
x.low <- setx(z.out, fract=300, numst2 = 0, crisis=200)
x.high <- setx(z.out, fract=300, numst2 = 1, crisis=200)

##  Simulating draws using the default method.
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()

## Simulating draws using bootstrap method.
user.prompt()
s.out1 <- sim(z.out, x=x.low, x1=x.high, bootstrap=T, num=10)
user.prompt()

##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
user.prompt()
plot(s.out)

user.prompt()
summary(s.out1)
user.prompt()
plot(s.out1)




