## Attaching the example dataset:
data(SupremeCourt)
names(SupremeCourt) <- c("Rehnquist","Stevens","OConnor","Scalia",
                         "Kennedy","Souter","Thomas","Ginsburg","Breyer")

user.prompt()

## Estimating the model using MCMCirt1d:
z.out <- zelig(cbind(Rehnquist,Stevens,OConnor,Scalia,
               Kennedy,Souter,Thomas,Ginsburg,Breyer)~NULL,
               data=SupremeCourt, model="irt1d",
               B0.alpha=0.2, B0.beta=0.2, burnin=500, mcmc=10000,
               thin=20, verbose=FALSE)
user.prompt()

## Checking for convergence before summarizing the estimates:
geweke.diag(z.out$coefficients)
user.prompt()

heidel.diag(z.out$coefficients)
user.prompt()


## summarizing the output
summary(z.out)







