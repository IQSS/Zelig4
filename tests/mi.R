library(Zelig)


#  Attaching the sample turnout dataset:
data(turnout)

#  Generating empirical estimates:
z.out1 <- zelig(
                vote ~ age,
                model = "logit",
                data = mi(turnout[1:10, ], turnout[100:110, ]),
                cite = FALSE
                )

#  Using setx to generate baseline and alternative velus for the
#  explanatory variables.  

x.out1 <- setx(z.out1, age = 90)

s.out1 <- sim(z.out1, x=x.out1, num=500)

summary(z.out1)
# summary(s.out1)
