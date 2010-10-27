zelig2.gamma <- function(model, formula, ..., data)
  alist(glm,
        "formula",
        "weights",
        "data",
        family = Gamma,
        model  = F
        )
