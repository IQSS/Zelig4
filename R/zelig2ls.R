#' Interface between ls model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2ls <- function(formula, ..., data, weights=NULL)
  list(
       .function = "lm",

       formula = formula,
       weights = weights,
       model   = F,
       data    = data
       )
