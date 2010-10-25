## Attaching the example dataset:
data(eidat)

## Estimating the model using MCMChierEI:
z.out <- zelig(cbind(t0, t1) ~ x0 + x1, model="ei.hier", data = eidat,
               mcmc = 40000, thin = 10, burnin = 10000, verbose = TRUE)
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

## Setting values for in-sample simulations given 
##  marginal values of X0, X1, T0 and T1:
x.out <- setx(z.out, fn = NULL, cond = TRUE)
user.prompt()             
         
## In-sample simulations from the posterior distribution:
s.out <- sim(z.out, x = x.out)

## Summarizing in-sample simulations at aggregate level
## weighted by the count in each unit:
summary(s.out)
user.prompt()

## Summarizing in-sample simulations at unit level 
## for the first 5 units:
summary(s.out, subset = 1:5)



















