zelig2normal <- function(model, formula, ..., data)
  list("glm",
       "formula",
       "data",
       "weights",
       family = gaussian,
       model  = F
       )
