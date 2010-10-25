###
### Example 1: Calculating the conditional average treatment effect
###            for the matched treatment group using nearest neighbor
###            propensity score matching
###

## load the Lalonde data
library(MatchIt)
data(lalonde)
user.prompt()

## propensity score matching
m.out1 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75, 
                  method = "nearest", data = lalonde)
user.prompt()

## fit the linear model to the entire sample controlling for propensity score and other covariates
z.out1 <- zelig(re78 ~ treat + age + educ + black + hispan + nodegree + married + re74 + re75 +
                       distance, data = match.data(m.out1), model = "ls")
user.prompt()

## set the covariates to the covariates using only matched treated units:
x.out0 <- setx(z.out1, data = match.data(m.out1, "treat"), fn = NULL, treat=0)
x.out1 <- setx(z.out1, data = match.data(m.out1, "treat"), fn = NULL)
user.prompt()

## simulate conditional average treatment effect for the treated
s.out1 <- sim(z.out1, x = x.out0, x1 = x.out1)
user.prompt()

## obtain a summary
summary(s.out1)
user.prompt()


###
### Example 2: Calculating the conditional average treatment effect
###            for the matched control group using nearest neighbor
###            propensity score matching
###


## set the covariates to the covariates using only matched control units:
x.out2 <- setx(z.out1, data = match.data(m.out1, "control"), fn = NULL)
x.out3 <- setx(z.out1, data = match.data(m.out1, "control"), fn = NULL, treat = 1)
user.prompt()

## simulate conditional average treatment effect for the treated
s.out2 <- sim(z.out1, x = x.out2, x1 = x.out3)
user.prompt()

## obtain a summary
summary(s.out2)
user.prompt()


###
### Example 3: Calculating the conditional average treatment effect
###            for the entire matched sample using nearest neighbor
###            propensity score matching
###

## set the covariates to the covariates using all matched units:
x.out4 <- setx(z.out1, fn = NULL, treat = 0)
x.out5 <- setx(z.out1, fn = NULL, treat = 1)
user.prompt()

## simulate conditional average treatment effect for the treated
s.out3 <- sim(z.out1, x = x.out4, x1 = x.out5)
user.prompt()

## obtain a summary
summary(s.out3)
user.prompt()


###
### Example 4: Calculating the conditional average treatment effect
###            for the entire sample using subclassification
###

## subclassification with 4 subclasses
m.out2 <- matchit(treat ~ age + educ + black + hispan + nodegree + married + re74 + re75,  
                  data = lalonde, method = "subclass", subclass = 4)
user.prompt()

## controlling only for the estimated prpensity score and lagged Y within each subclass
## one can potentially control for more
z.out2 <- zelig(re78 ~ treat + re74 + re75 + distance, data = match.data(m.out2), 
                model = "ls", by = "subclass")
user.prompt()

## conducting simulations
x.out6 <- setx(z.out2, fn = NULL, treat = 0)
x.out7 <- setx(z.out2, fn = NULL, treat = 1)
user.prompt()

## for the demonstration purpose, we set the number of simulations to be 100
s.out4 <- sim(z.out2, x = x.out6, x1 = x.out7, num = 100)
user.prompt()

## overall results
summary(s.out4) 
user.prompt()

## summary for each subclass
summary(s.out4, subset = 1) 
user.prompt()

summary(s.out4, subset = 2) 
user.prompt()

summary(s.out4, subset = 3) 


