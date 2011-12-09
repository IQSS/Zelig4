#' Get Response Terms from a List-style Formula
#'
#' This method gets the response terms from a standard formula
#' @usage \method{getResponseTerms}{list}(x, ...)
#' @param x a list of formulae
#' @param ... ignored parameters
#' @return a character-vector specifying the response terms of the formula
#' @S3method getResponseTerms list
#' @author Matt Owen
getResponseTerms.list <- function (x, ...) {
  if (! all(unlist(Map(is.formula, x)))) {
    # If not all the elements are formulae, then we should strip them from 'x'
    warning("All non-formula will be removed from this list.")

    x <- Filter(is.formula, x)
  }

  if (length(x) == 0)
    # Zero-sized lists will have no available response terms, and should thus
    # return a zero-length character vector. Note this is intended to ensure
    # the result of 'getResponseTerms' is always a character-string.
    vector("character", 0)

  else
    # Get response terms of each element of 'x',
    # then transform the list into a vector, which should always be flat, since
    # getResponseTerms should always return a character-string
    unlist(Map(getResponseTerms, x, single.only=TRUE))
}
