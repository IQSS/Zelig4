## Attaching the example dataset:
data(SupremeCourt)
names(SupremeCourt) <- c("Rehnquist","Stevens","OConnor","Scalia",
                         "Kennedy","Souter","Thomas","Ginsburg","Breyer") 
user.prompt()

## Estimating the model using MCMCirtKd:
z.out <- zelig(cbind(Rehnquist,Stevens,OConnor,Scalia,
               Kennedy,Souter,Thomas,Ginsburg,Breyer)~NULL,
               dimensions=1, data=SupremeCourt, model="irtkd",
               B0=0.25, burnin=5000, mcmc=50000, thin=10, verbose=TRUE)
               
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








