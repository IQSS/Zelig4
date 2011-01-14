zelig2logit <- function(model, formula, ..., data, weights=NULL)
  list("glm",
       "formula",
       "data",
       "weights",
       family = binomial(link="logit"),
       model  = F
       )
