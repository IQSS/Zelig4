# exp
# exp
# exp

# Fit the statistical model

data(coalition)

z.out <- zelig(Surv(duration, ciep12) ~ invest + polar + numst2 + crisis, model = "exp", data = coalition)

user.prompt()

# Set explanatory variables

x.low<- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

user.prompt()

# Simulate quantities of interest

s.out <- sim(z.out, x = x.low, x1 = x.high, num = 10)
summary(s.out)

user.prompt()

# Plot simualted results

plot(s.out)
