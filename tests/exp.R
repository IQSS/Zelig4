library(Zelig)

data(coalition)

z.out <- zelig(
               Surv(duration, ciep12) ~ invest + polar + numst2 + crisis,
               model = "exp",
               data = coalition
               )

summary(z.out)

x.low<- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

s.out <- sim(z.out, x = x.low, x1 = x.high)

summary(s.out)

plot(s.out)
