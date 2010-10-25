## Attaching the example dataset:
data(Weimar)

## Estimating the model using eiRxC:
z.out1 <- zelig(cbind(Nazi, Government, Communists, FarRight, Other) ~  
		shareunemployed + shareblue + sharewhite + shareself + 
		sharedomestic, model = "ei.RxC", data = Weimar)

## summarizing the output
summary(z.out1)
user.prompt()
                      
## In-sample simulations from the posterior distribution:
s.out1 <- sim(z.out1, num =10)

## Summarizing in-sample simulations at aggregate level
## weighted by the count in each unit:
summary(s.out1)
user.prompt()

################# using covariate


## Attaching the example dataset:
data(Weimar)

## Estimating the model using eiRxC:
z.out2 <- zelig(cbind(Nazi, Government, Communists, FarRight, Other) ~  
		shareunemployed + shareblue + sharewhite + shareself + 
		sharedomestic, 
                covar = ~ shareprotestants, 
                model = "ei.RxC", data = Weimar)


## summarizing the output
summary(z.out2)
user.prompt()

## Setting values for in-sample simulations given 
##  marginal values of X0, X1, T0 and T1:
x.out2 <- setx(z.out2)
user.prompt()
                      
## In-sample simulations from the posterior distribution:
s.out2 <- sim(z.out2, num = 10)

## Summarizing in-sample simulations at aggregate level
## weighted by the count in each unit:
summary(s.out2)

 



















