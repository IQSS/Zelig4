library(Zelig)

##  Attaching the sample turnout dataset:
data(turnout)

##  Generating empirical estimates:
z.out1 <- zelig(vote ~ age,
                model = "logit",
                data = mi(turnout[1:500,], turnout[501:1000,]),
                by = "race",
                cite = FALSE
                )

##  Using setx to generate baseline and alternative velus for the
##  explanatory variables.  

x.out1 <- setx(z.out1, age = 36, race = "white")

s.out <- sim(z.out1, x = x.out1)

# Compute summaries for the fitted models

Map(summary, z.out1$result)

# Summary of multiply-imputed simulations of interest

summary(s.out)
