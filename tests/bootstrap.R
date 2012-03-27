library(Zelig)

data(coalition)

z.out <- zelig(duration ~ fract + numst2 + crisis,
               model = "gamma", 
               data = coalition
               )

x.low <- setx(z.out, fract=300, numst2 = 0, crisis=200)
x.high <- setx(z.out, fract=300, numst2 = 1, crisis=200)

## Simulating draws using bootstrap method.
s.out <- sim(z.out, x = x.low, x1 = x.high, num = 10, bootstrap = TRUE)

##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
