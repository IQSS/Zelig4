zelig2gamma <- function(model, formula, ..., data)
  list("glm",
       "formula",
       "weights",
       "data",
       family = Gamma,
       model  = F
       )
