library(Zelig)

# Single outcome term, single set of response terms
message("Single outcome term")

Zelig:::getResponseTerms(y ~ 1)
Zelig:::getResponseTerms(x + y ~ 1)
Zelig:::getResponseTerms(f(x) ~ 1)
Zelig:::getResponseTerms(f(x) + y ~ 1)


# Multiple outcome terms, single set of response terms
message("Multiple outcome terms, single set of response terms")

Zelig:::getResponseTerms(cbind(x) ~ 1)
Zelig:::getResponseTerms(list(x, y) ~ 1)
Zelig:::getResponseTerms(cbind(x, sin(y) + x) ~ 1)


# Multiple outcome terms, multiple sets of response terms
message("Multiple outcome terms, multiple sets of response terms")

Zelig:::getResponseTerms(list(x ~ 1))
Zelig:::getResponseTerms(list(x ~ 1,
                                      f(y) ~ x1 + x2 + x3
                                      )
                                 )

# Invalid Formulae
message("Invalid formulae")
Zelig:::getResponseTerms(cbind(cbind(x, y), x) ~ 1)
Zelig:::getResponseTerms(cbind( , x) ~ 1)
Zelig:::getResponseTerms(list())
Zelig:::getResponseTerms(cbind(x, sin(y) + x) ~ 1, single.only=TRUE)
Zelig:::getResponseTerms(list(x ~ 1,
                                      f(y) ~ x1 + x2 + x3,
                                      cbind(x, y) ~ 1
                                      )
                                 )

