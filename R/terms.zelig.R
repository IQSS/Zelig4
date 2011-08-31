#' Model Terms for a Zelig Object
#' 
#' This method simply extracts the model terms for the fitted model passed to 
#' the \code{zelig} function.
#' @S3method terms zelig
#' @usage \method{terms}{zelig}(x, ...)
#' @param x a \code{zelig} object
#' @param ... forwarded parameters
#' @return terms of the original fitted model
terms.zelig <- function (x, ...) {
  terms(x$result, ...)
}
