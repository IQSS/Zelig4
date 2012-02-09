library(Zelig)
library(Formula)

data(turnout)

fff <- Formula(vote | income ~ educate | race + 0)


pf <- parseFormula(fff, turnout[1, ])

formula(pf)
pf$model.matrix
