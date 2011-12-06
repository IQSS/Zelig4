library(Zelig)

model <- "sur"


form1 <- list(mu1=Ige~Fge+Cge, mu2=Iw~Fw+Cw)


parsed1 <- parse.formula(form1, model)

terms(parsed1)
