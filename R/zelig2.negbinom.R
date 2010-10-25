zelig2.negbinom <- function(model, formula, ..., data)
  alist(MASS::glm.nb,
        formula = formula,
        "weights",
        "data"
        )
