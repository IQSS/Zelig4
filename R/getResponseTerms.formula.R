#' Get Response Terms from a Standard Formula
#'
#' This method gets the response terms from a standard formula
#' @param x a formula
#' @param ... ignored parameters
#' @param single.only a logical specifying whether 'cbind' or 'list' keywords
#' are allowed
#' @param duplicates a logical specifying whether the returned character-vector
#' will only return duplicates.
#' @return a character-vector specifying the response terms of the formula
#' @S3method getResponseTerms formula
#' @author Matt Owen
getResponseTerms.formula <- function (x, ..., single.only=FALSE, duplicates=TRUE)
{

  if (length(x) < 3)
    # If the length is less than 3, there are no response terms
    return(NA)

  else if (length(x) > 3)
    # If the length is greater than 3, something really weird is going on.
    stop("Single formulae must be constructed like ",
         "call objects (have length 3).")


  # Reponse terms are always specified in the lefthand-side of the equation
  lhs <- x[[2]]


  if (is.name(lhs))
    # If the lhs is a name, this implies it's a single variable with no function
    # applied to it. Thus, it's a term.
    return(toString(lhs))


  # Otherwise, it is either a function being applied or the keywords "cbind" or
  # "list"
  op <- toString(lhs[[1]])

  if (op %in% c("cbind", "list")) {

    if (single.only) {
      # If only single outcome response terms are allowed, then 'cbind' and
      # 'list' cannot be used.
      warning("'cbind' and 'list' may not be used ",
              "in this formula specification.")
      return(vector("character", 0))
    }

    # If it is one of the keywords, we extract these terms individually
    lis <- as.list(lhs[-1])
    lis <- unlist(Map(toString, lis))

    if (!duplicates)
      # If duplicates flag is FALSE, remove all duplicate entries
      lis <- unique(lis)

    # Remove all emptry strings and return
    Filter(nchar, lis)

  }

  else {
    # Otherwise, we can treat them as one single term. That is the formula:
    #   x + y ~ 1
    # will have a single response term:
    #   x + y
    toString(lhs)
  }
}
