zelig2normal <- function(model, formula, weights=NULL, ..., data)
  list(
       .function = "glm",

       formula = formula,
       weights = weights,
       family  = gaussian,
       model   = F,
       data    = data
       )
