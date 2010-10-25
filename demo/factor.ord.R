## Attaching the example dataset:
data(newpainters)

## Estimating the model using factor.ord:
z.out <- zelig(cbind(Composition,Drawing,Colour,Expression)~NULL,   
                    data=newpainters, model="factor.ord",  
                    factors=1,
                    burin=5000,mcmc=30000, thin=5, verbose=TRUE,
                    L0=0.5,tune=1.2)

user.prompt()

## Checking for convergence before summarizing the estimates:
geweke.diag(z.out$coefficients)
user.prompt()

heidel.diag(z.out$coefficients)
user.prompt()

## summarizing the output
summary(z.out)







