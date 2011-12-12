library(Zelig)

model <- "blogit"

data(sanction)

form1 <- cbind(import, export) ~ coop + cost + target
form2 <- list(mu1 = import ~ sin(coop), mu2 = export~cost+target)
form3 <- sin(import) ~ 1 + coop

# parsed1 <- parseFormula(form1, model)
# parsed2 <- parseFormula(form2, model)
parsed3 <- parseFormula(form3, model)

# terms(parsed1)
# terms(parsed2)

# parsed1$response
# parsed2$response
#parsed3$response

# getResponseTerms(form1)
# getResponseTerms(form2)
# getResponseTerms(form3)

message("#1")
makeModelMatrix(form1, sanction)[1, ]

message("#2")
makeModelMatrix(form2, sanction)[1, ]

message("#3")
makeModelMatrix(form3, sanction)[1, ]
