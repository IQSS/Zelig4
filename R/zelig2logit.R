zelig2logit <- function(model, formula, weights=NULL, ..., data)
  list(
       .function = "glm",

       formula = formula,
       weights = weights,
       family  = binomial(link="logit"),
       model   = F,
       data    = data
       )
