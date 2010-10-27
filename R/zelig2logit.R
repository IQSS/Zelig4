zelig2.logit <- function(model, formula, ..., data, weights=NULL)
  alist(glm,
        "formula",
        "data",
        "weights",
        family = binomial(link="logit"),
        model  = F
        )
