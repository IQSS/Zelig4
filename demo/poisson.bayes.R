## Attaching the example dataset:
data(sanction)

## Estimating the model using poisson.bayes:
z.out <- zelig(num ~ target + coop, model = "poisson.bayes",
                  data = sanction, verbose=TRUE)
user.prompt()

## Checking for convergence before summarizing the estimates:
geweke.diag(z.out$result$coefficients)
user.prompt()

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
## values, with the number of targets to be its maximum 
## versus its minimum:
x.max <- setx(z.out, target = max(sanction$target))
x.min <- setx(z.out, target = min(sanction$target))
user.prompt()

## Estimating the first difference for the effect of
## maximum versus minimum number of targets:
s.out2 <- sim(z.out, x = x.max, x1 = x.min)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








