## Attaching the example dataset:
data(swiss)
names(swiss) <- c("Fert","Agr","Exam","Educ","Cath","InfMort")

user.prompt()

## Estimating the model using MCMCfactanal:
z.out <- zelig(cbind(Agr,Exam,Educ,Cath,InfMort)~NULL, 
	       model="factor.bayes",
               data=swiss, factors=2,
               lambda.constraints=list(Exam=list(1,"+"),
                                 Exam=list(2,"-"), Educ=c(2,0),
                                 InfMort=c(1,0)),
               verbose=TRUE, a0=1, b0=0.15,
               burnin=5000, mcmc=50000)
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








