zelig2.poisson <- function(model, formula, ..., data)
  alist(glm,
        "formula",
        "weights",
        "data",
        family = poisson,
        model  = F
        )
