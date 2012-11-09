## Attaching the example dataset:
data(sanction)

# Create an ordered dependent variable: 
user.prompt()
sanction$ncost <- factor(sanction$ncost, ordered = TRUE,
                         levels = c("net gain", "little effect", 
                         "modest loss", "major loss"))

## Estimating the model using oprobit.bayes:
z.out <- zelig(ncost ~ mil + coop, model = "oprobit.bayes",
                  data = sanction, verbose=FALSE, tune=0.3)

user.prompt()

## Checking for convergence before summarizing the estimates:
#geweke.diag(z.out$coefficients)
#user.prompt()

heidel.diag(z.out$result$coefficients)
user.prompt()

raftery.diag(z.out$result$coefficients)
user.prompt()

## summarizing the output
summary(z.out)
user.prompt()

## Setting values for the explanatory variables to 
## their sample averages:
x.out <- setx(z.out)
user.prompt()

## Simulating quantities of interest from the posterior 
## distribution given x.out:
s.out1 <- sim(z.out, x = x.out)
user.prompt()

## Summarizing the simulation results:
summary(s.out1)
user.prompt()

## Simulating First Differences:
## Setting explanatory variables to their default(mean/mode)
## values, with military action to be yes(1) or no(0)
x.high <- setx(z.out, mil=0)
x.low <- setx(z.out, mil=1)
user.prompt()

## Estimating the first difference for the effect of
## military action on the probabilities of
## incurring differnt level of cost:

s.out2 <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








