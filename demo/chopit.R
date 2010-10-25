data(free1, free2) 

## Setting up the formula as a list for the self-response,
##  vignettes, and the cut points (drawn from both the self-response
##  and vignette data sets).  
formulas <- list(self = y ~ sex + age + factor(country),
                 vign = cbind(v1, v2, v3, v4, v5) ~ 1,
                 tau  = ~ sex + age + factor(country))

## Setting up the data as a list, one data set corresponding to the
##  self-response, and one to the vignette responses.  Note that the
##  tau variables must be in both data sets.  
data <- list(self = free1, vign = free2)
z.out <- zelig(formulas, data = data, model = "chopit")
user.prompt()

## Using defaults
x.out1 <- setx(z.out)
s.out1 <- sim(z.out, x = x.out1)
summary(s.out1)
user.prompt()

## Calculating first differences
x.out2 <- setx(z.out, age = 25)
s.out2 <- sim(z.out, x = x.out1, x1 = x.out2)
summary(s.out2)
user.prompt()

## Conditional predication in this case calculates E(mu|X,Y).
##  This procedure involves numeric integration, which takes
##  approximately 1 second per observation on 64-bit R.  
x.out3 <- setx(z.out, cond = TRUE)
s.out3 <- sim(z.out, x = x.out3)
user.prompt()
summary(s.out3) 

