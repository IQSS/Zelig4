library(Zelig)

model <- "twosls"


form1 <- list(
              mu1=C~Wtot + P + P1,
              mu2=I~P + P1 + K1,
              mu3=Wp~ X + X1 + Tm
              )


parsed1 <- parse.formula(form1, model)

terms(parsed1)
