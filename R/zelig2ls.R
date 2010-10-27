zelig2.ls <- function(model, formula, ..., data, weights=NULL)
  alist(lm,
        model = F,
        "formula",
        "weights",
        "data"
        )
