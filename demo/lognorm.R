library(ZeligCommon)
# Load the sample data:  
data(coalition)

# Estimate the model:
user.prompt()
z.out <- zelig(Surv(duration, ciep12) ~ fract + numst2, model = "lognorm",
               data = coalition)
user.prompt()
# View the regression output:  
summary(z.out)

# Set the baseline values (with the ruling coalition in the minority)
# and the alternative values (with the ruling coalition in the majority)
# for X:
user.prompt()
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

# Simulate expected values qi$ev and first differences qi$fd:
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()
summary(s.out)
user.prompt()
plot(s.out)
