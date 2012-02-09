#' Get Response Terms from a Standard Formula
#'
#' This method gets the response terms from a standard formula
#' @usage
#' \method{getResponseTerms}{formula}(x, ..., single.only=FALSE, duplicates=TRUE)
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
  # Handle 
  handle.formula.err <- function (e) {
    message("\n\n")
    message("The formula ", x, " seems to have no dependent variables")
    stop("The formula for the ")
  }

  rhs <- tryCatch(x[[3]], error = handle.formula.err)
  lhs <- tryCatch(x[[2]], error = handle.formula.err)

  # Reponse terms are always specified in the lefthand-side of the equation
  if (is.name(lhs)) {
    # If the lhs is a name, this implies it's a single variable with no function
    # applied to it. Thus, it's a term.
    return(tryCatch(
                    toString(lhs),
                    error = function (e) as.character(lhs)
           ))
  }

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
