library(Zelig)

library(MCMCpack)

data(turnout)
data(mexico)
data(macro)
data(sanction)

# mlogit.bayes
# mlogit.bayes
# mlogit.bayes

z.out <- zelig(
               vote88 ~ pristr + othcok + othsocok,
               model = "mlogit.bayes", 
               data = mexico
               )

x.out <- setx(z.out)

s.out <- sim(z.out, x = x.out)

summary(z.out)
summary(s.out)

# logit.bayes
# logit.bayes
# logit.bayes

names(swiss) <- c("Fert","Agr","Exam","Educ","Cath","InfMort")

z.out <- zelig(
               vote ~ race + educate,
               model = "logit.bayes",
               verbose = FALSE,
               data  = turnout
               )
summary(z.out)

x.out <- setx(z.out, age=65)
x1.out <- setx(z.out, age=10, educate=5)

s.out <- sim(z.out, x.out, x1.out)

summary(s.out)

# normal.bayes
# normal.bayes
# normal.bayes

z.out <- zelig(
               unem ~ gdp + capmob + trade,
               model = "normal.bayes", 
               data = macro,
               verbose=TRUE
               )

x.out <- setx(z.out)
x1.out <- setx(z.out, gdp = 10)

s.out <- sim(z.out, x.out, x1.out)

summary(z.out)
summary(s.out)

sanction$ncost <- factor(sanction$ncost, ordered = TRUE,
                         levels = c("net gain", "little effect", 
                         "modest loss", "major loss"))


z.out <- zelig(
               ncost ~ mil + coop,
               model = "oprobit.bayes",
               data = sanction, verbose=FALSE
               )

x.out <- setx(z.out)
x1.out <- setx(z.out, coop=3)

s.out <- sim(z.out, x = x.out, num=10000)

summary(z.out)
summary(s.out)

z.out <- zelig(
               num ~ target + coop, 
               model = "poisson.bayes",
               data = sanction, 
               verbose=TRUE
               )

x.out <- setx(z.out)
x1.out <- setx(z.out, coop=3)

s.out <- sim(z.out, x.out, x1.out)

summary(z.out)
summary(s.out)

z.out <- zelig(
               vote ~ race + educate,
               model = "probit.bayes",
               verbose = FALSE,
               data  = turnout
               )

x.out <- setx(z.out, age=65)
x1.out <- setx(z.out, age=10, educate=5)

s.out <- sim(z.out, x.out, x1.out)

summary(s.out)

# YOU NEVER WAIT SO LONG!















z.out <- zelig(cbind(Agr,Exam,Educ,Cath,InfMort)~NULL, 
               model="factor.bayes",
               data=swiss, factors=2,
               lambda.constraints=list(Exam=list(1,"+"),
                                 Exam=list(2,"-"), Educ=c(2,0),
                                 InfMort=c(1,0)),
               verbose=TRUE, a0=1, b0=0.15,
               burnin=5000, mcmc=10000)

## Checking for convergence before summarizing the estimates:
geweke.diag(coef(z.out))
heidel.diag(coef(z.out))
raftery.diag(coef(z.out))

## summarizing the output
summary(z.out)

# These methods should not work.
#setx(z.out)
#sim(z.out)



