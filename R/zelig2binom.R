zelig2negbinom <- function(model, formula, ..., data)
  alist(glm.nb,
        "formula",
        "weights",
        "data"
        )
