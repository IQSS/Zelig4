## Attaching the example dataset:
data(mexico)

## Estimating the model using mlogit.bayes:
z.out <- zelig(vote88 ~ pristr + othcok + othsocok, model = "mlogit.bayes", 
               data = mexico)
user.prompt()

## Checking for convergence before summarizing the estimates:
heidel.diag(z.out$result$coefficients)
user.prompt()

raftery.diag(z.out$result$coefficients)
user.prompt()

## Summarizing the output
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
## values, with pristr(the strength of PRI) equal to 1(weak) or
## 3(strong)
x.weak <- setx(z.out, pristr = 1)
x.strong <- setx(z.out, pristr = 3)

user.prompt()

## Estimating the first difference for the effect of
## military action on the probabilities of
## incurring differnt level of cost:
s.out2 <- sim(z.out, x = x.strong, x1 = x.weak)
user.prompt()

## Summarizing the simulation results:
summary(s.out2)








