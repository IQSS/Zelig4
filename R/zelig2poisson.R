zelig2poisson <- function(model, formula, weights=NULL, ..., data)
  list(
       .function ="glm",
       
       formula = formula,
       weights = weights,
       family  = poisson(),
       model   = F,
       data    = data
       )
