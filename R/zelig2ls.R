zelig2ls <- function(model, formula, ..., data, weights=NULL)
  list(
       .function = "lm",

       formula = formula,
       weights = weights,
       model   = F,
       data    = data
       )
