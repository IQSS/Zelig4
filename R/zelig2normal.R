zelig2.normal <- function(model, formula, ..., data)
  alist(glm,
        "formula",
        "data",
        "weights",
        family = gaussian,
        model  = F
        )
