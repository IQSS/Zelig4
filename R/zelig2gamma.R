zelig2gamma <- function(model, formula, ..., data)
  alist(glm,
        "formula",
        "weights",
        "data",
        family = Gamma,
        model  = F
        )
