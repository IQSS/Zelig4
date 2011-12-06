#' Get Response Terms from a Zelig-style Formula
#'
#' This method acquires the response variables from Zelig-style input.
#' @param x a formula or list of formulae
#' @param ... ignored parameters
#' @return a character-vector specifying a the of response terms in this formula
getResponseTerms <- function (x, ...) {
  UseMethod("getResponseTerms")
}
