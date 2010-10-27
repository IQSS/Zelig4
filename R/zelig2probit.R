zelig2probit <- function(model, formula, ..., data)
  alist(glm,
        "formula",
        "data",
        "weights",
        family = binomial(link="probit"),
        model  = F
        )
