## Attaching the example dataset:
data(turnout)

## Estimating the model using MCMClogit:
z.out <- zelig(vote ~ race + educate, model = "logit.bayes",
                  data = turnout, verbose=TRUE)
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
## Setting education is set to be between low(25th percentile) 
## versus high(75th percentile) while all the other variables 
## held at their default values.
x.high <- setx(z.out, educate = quantile(turnout$educate, prob = 0.75))
x.low <- setx(z.out, educate = quantile(turnout$educate, prob = 0.25))
user.prompt()

## Estimating the first difference for the effect of
## high versus low trade on unemployment rate:
s.out2 <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








