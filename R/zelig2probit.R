zelig2probit <- function(model, formula, weights=NULL, ..., data)
  list(
       .function = "glm",

       formula = formula,
       weights = weights,
       family  = binomial(link="probit"),
       model   = F,
       data    = data
       )
