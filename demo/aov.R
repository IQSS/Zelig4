 ## From Venables and Ripley (2002) p.165.
### Attach data and set contrasts
data(npk, package="MASS")
 op <- options(contrasts=c("contr.helmert", "contr.poly"))
user.prompt()
z.out1 <- zelig(formula=yield ~ block + N*P*K, model="aov", data=npk)
user.prompt()
summary(z.out1)
user.prompt()
###Set explanatory variables
 x <- setx(z.out1)
###Simulate model at explanatory variables 
user.prompt()
s.out1 <- sim(z.out1, x = x)
user.prompt()
plot(s.out1)
user.prompt()
###Example with Error term
z.out2 <- zelig(yield ~  N*P*K + Error(block), model="aov",data=npk)
user.prompt()
summary(z.out2)
user.prompt()
 x <- setx(z.out2)
user.prompt()
s.out2 <- sim(z.out2, x=x)
user.prompt()
plot(s.out2)
###Reset previous contrasts
options(op)
### Use data set oats from MASS
 z.out3 <- zelig(Y ~ N*V + Error(B/V), model="aov", data=oats)
user.prompt()
summary(z.out3)
user.prompt()
 x.out <- setx(z.out3, N="0.0cwt", V="Golden.rain")
x.out1 <- setx(z.out3, N="0.0cwt", V="Victory")
user.prompt()
s.out3 <- sim(z.out3, x = x.out,x1=x.out1)
summary(s.out3)
user.prompt()
 plot(s.out3)





        
