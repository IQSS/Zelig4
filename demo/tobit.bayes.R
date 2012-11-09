## Attaching the example dataset:
data(tobin)

## Estimating the model using tobit.bayes:
z.out <- zelig(durable ~ age + quant, model = "tobit.bayes",
                  data = tobin, verbose=TRUE)
user.prompt()

## Checking for convergence before summarizing the estimates:
geweke.diag(z.out$coefficients)
user.prompt()

heidel.diag(z.out$coefficients)
user.prompt()

raftery.diag(z.out$coefficients)
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
## values, with high (80th percentile) and low (20th percentile) 
## liquidity ratio(\texttt{quant}):
x.high <- setx(z.out, quant = quantile(tobin$quant, prob = 0.8))
x.low <- setx(z.out, quant = quantile(tobin$quant, prob = 0.2)) 
user.prompt()

## Estimating the first difference for the effect of
## high versus low liquidity ratio:
s.out2 <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








