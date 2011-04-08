#' Interface between gamma model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2gamma <- function(formula, ..., data)
  list(
       .function = "glm",

       formula = formula,
       family  = Gamma,
       model   = F,
       data    = data
       )
