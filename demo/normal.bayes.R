## Attaching the example dataset:
data(macro)

## Estimating the model using normal.bayes:
z.out <- zelig(unem ~ gdp + capmob + trade, model = "normal.bayes", 
                  data = macro, verbose=TRUE)
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

## Simulating First Differences
## Set explanatory variables to their default(mean/mode) values, 
## with high (80th percentile) and low (20th percentile) trade on GDP:
x.high <- setx(z.out, trade = quantile(macro$trade, prob = 0.8))
x.low <- setx(z.out, trade = quantile(macro$trade, prob = 0.2))
user.prompt()

## Estimating the first difference for the effect of
## high versus low trade on unemployment rate:
s.out2 <- sim(z.out, x = x.high, x1 = x.low)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








