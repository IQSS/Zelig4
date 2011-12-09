#' Whether an Object is a Formula
#' 
#' This is a boolean-check to see whether an object is a formula.
#' @note This will not be shared in the Zelig/ZeligFormulae namespace.
#' @param x an object
#' @return a logical specifying whether an object is a formula
#' @author Matt Owen
is.formula <- function (x)
  "formula" %in% class(x)
