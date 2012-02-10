library(Zelig)
library(Formula)

# No predictors
message("No predictors/No intercept")
Zelig:::getPredictorTerms(x ~ 0)
Zelig:::getPredictorTerms(Formula(x ~ 0 | 0))
Zelig:::getPredictorTerms(list(x ~ 0))
Zelig:::getPredictorTerms(list(x ~ 0, y ~ 0))

#
message()

# Single outcome term, single set of response terms
message("Single Equation")

Zelig:::getPredictorTerms(y ~ x + y*x + 1 + sin(y))
Zelig:::getPredictorTerms(x + y ~ x + 1)
Zelig:::getPredictorTerms(Formula(y ~ x + 0))
Zelig:::getPredictorTerms(Formula(f(x) ~ I(x*x) + 1))

#
message()

# Multiple outcome terms, single set of response terms
message("Multiple outcome terms, single set of response terms")

Zelig:::getPredictorTerms(cbind(x) ~ 1)
Zelig:::getPredictorTerms(list(x, y) ~  x + y + 1)
Zelig:::getPredictorTerms(cbind(x, sin(y) + x) ~ x+ 1)
Zelig:::getPredictorTerms(Formula(x | y ~ 1))
Zelig:::getPredictorTerms(Formula(f(x) | x ~ x + 0))
Zelig:::getPredictorTerms(Formula(f(x) | g(y) ~ I(x*x) + 1))

#
message()

# Multiple outcome terms, multiple sets of response terms
message("Multiple outcome terms, multiple sets of response terms")

Zelig:::getPredictorTerms(list(x ~ x + z + 1, y ~ x*y + 0))
Zelig:::getPredictorTerms(list(Formula(x | y ~ x + 1 | z*a), zzz ~ xxx))
Zelig:::getPredictorTerms(list(Formula(f(x) | y ~ sin(x) + 1 | champs + 0)))
Zelig:::getPredictorTerms(list(
                               x ~ sin(x) + 1,
                               f(y) ~ x1 + x2 + x3
                          ))

#
message()

# 'Formula' object specific tests
message("'Formula' specific")

Zelig:::getPredictorTerms(Formula(x | y ~ 1))
Zelig:::getPredictorTerms(Formula(x ~ 1))


# Invalid Formulae
message("Invalid formulae")

Zelig:::getPredictorTerms(cbind(cbind(x, y), x) ~ 1)
Zelig:::getPredictorTerms(cbind( , x) ~ 1)
Zelig:::getPredictorTerms(list())
Zelig:::getPredictorTerms(cbind(x, sin(y) + x) ~ 1, single.only=TRUE)
Zelig:::getPredictorTerms(list(x ~ 1,
                                      f(y) ~ x1 + x2 + x3,
                                      cbind(x, y) ~ 1
                                      )
                                 )

