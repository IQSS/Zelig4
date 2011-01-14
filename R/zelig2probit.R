zelig2probit <- function(model, formula, ..., data)
  list("glm",
       formula = formula,
       data = data,
       weights = weights,
       family = binomial(link="probit"),
       model  = F
       )
