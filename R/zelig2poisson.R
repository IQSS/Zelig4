zelig2poisson <- function(model, formula, ..., data)
  list("glm",
       "formula",
       "weights",
       "data",
       family = poisson,
       model  = F
       )
