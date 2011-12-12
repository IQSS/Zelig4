library(Zelig)

model <- "mlogit"


form1 <- as.factor(vote88) ~ pristr + othcok + othsocok
form2 <- list(id(vote88,"1")~pristr + othcok, id(vote88,"2")~othsocok)


parsed1 <- parseFormula(form1, model)
#parsed2 <- parseFormula(form2, model)

terms(parsed1)
