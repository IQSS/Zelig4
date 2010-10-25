data(voteincome)
user.prompt()
z.out <- zelig(vote ~ education + age + female + tag(1 | state),
                   data=voteincome, model="logit.mixed")
user.prompt()
summary(z.out)

##  Setting the explanatory variables at their default values
##  (mode for factor variables and mean for non-factor variables),
##  with education set to 80th and 20th percentiles.
user.prompt()
x.low <- setx(z.out, education=quantile(voteincome$education, 0.8))
x.high <- setx(z.out, education=quantile(voteincome$education, 0.2))

##  Simulating draws using the default bootstrap method.
user.prompt()
s.out <- sim(z.out, x = x.low, x1 = x.high)
user.prompt()

##  Viewing the simulated quantities of interest, for every
##  observation:
summary(s.out)
user.prompt()
