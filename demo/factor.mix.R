## Attaching the example dataset:
data(PErisk)

## Estimating the model using factor.mix:
z.out <- zelig(cbind(courts,barb2,prsexp2,prscorr2,gdpw2)~NULL, 
		data=PErisk, model="factor.mix",factors=1, 
             	burnin=5000,mcmc=100000, thin=50, verbose=TRUE,
                L0=0.25,tune=1.2)
user.prompt()

## Checking for convergence before summarizing the estimates:
geweke.diag(z.out$coefficients)
user.prompt()
heidel.diag(z.out$coefficients)
user.prompt()


## summarizing the output
summary(z.out)








