zelig2gamma <- function(model, formula, ..., data)
  list(
       .function = "glm",

       formula = formula,
       family  = Gamma,
       model   = F,
       data    = data
       )
