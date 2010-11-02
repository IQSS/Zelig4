library(Zelig)


##  Attaching the sample turnout dataset:
data(turnout)

#####  Example 1:  Simple Example 

##  Generating empirical estimates:
z.out1 <- zelig(vote ~ age,
                model = "logit",
                data = mi(turnout[1:500,], turnout[501:1000,]),
                by = "race",
                cite = FALSE
                )


vcov(z.out1)
summary(z.out1)
z.out1 <- zelig(vote ~ age,
                model = "logit",
                data = mi(turnout[1:500,], turnout[501:1000,]),
                cite = FALSE
                )

vcov(z.out1)


z.out1 <- zelig(vote ~ age,
                model = "logit",
                data = turnout,
                cite = FALSE
                )
vcov(z.out1)


z.out1 <- zelig(vote ~ age,
                model = "logit",
                data = mi(turnout),
                cite = FALSE
                )
vcov(z.out1)

z.out1

##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  

x.out1 <- setx(z.out1, age = 36, race = "white")
x.out1
