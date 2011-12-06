
library(Zelig)

form1 <- cbind(import, export) ~ coop + cost + target
form1.2 <- list(import ~ coop + cost + target, export ~ coop + cost + target)
form2 <- list(mu1=import~coop, mu2=export~cost+target)

parsed1 <- parseFormula(form1)
parsed1.2 <- parseFormula(form1.2)
#parsed2 <- parseFormula(form2)

terms(parsed1)
#terms(parsed1.2)
