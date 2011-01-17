zelig2negbinom <- function(model, formula, weights=NULL, ..., data)
  list(
       .function = "glm.nb",
       weights = weights,
       formula = formula,
       data    = data
       )
