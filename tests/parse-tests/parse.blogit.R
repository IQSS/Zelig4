
library(Zelig)

model <- "blogit"


form1 <- cbind(import, export) ~ coop + cost + target
form2 <- list(mu1=import~coop, mu2=export~cost+target)

parsed1 <- parse.formula(form1, model)
parsed2 <- parse.formula(form2, model)

terms(parsed1)
