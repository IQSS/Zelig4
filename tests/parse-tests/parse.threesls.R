library(Zelig)

model <- "threesls"


form1 <- list(
              mu1 = q ~ p + d,
              mu2 = q ~ p + f + a
              )



parsed1 <- parse.formula(form1, model)

terms(parsed1)
