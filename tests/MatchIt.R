library(MatchIt)
library(Zelig)

data(lalonde)

m <- matchit(
             treat ~ age + educ + black + hispan + nodegree + married + re74 + re75,
             data = lalonde,
             method = "subclass",
             subclass = 4
             )

z <- zelig(re78 ~ re74 + re75 + distance, 
           data = match.data(m, "control"), 
           model = "ls",
           by = "subclass"
           )

# Fin.
