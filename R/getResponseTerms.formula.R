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



#' Get Response Terms from a ``Formula'' Object
#'
#' This method gets the response terms from a ``Formula'' Object
#' @rdname getResponse.Formula-alternate
#' @aliases getResponse.Formula
#' @usage
#' \method{getResponseTerms}{Formula}(x, ..., single.only=FALSE, duplicates=TRUE)
#' @param x a formula
#' @param ... ignored parameters
#' @param single.only a logical specifying whether 'cbind' or 'list' keywords
#' are allowed
#' @param duplicates a logical specifying whether the returned character-vector
#' will only return duplicates.
#' @return a character-vector specifying the response terms of the formula
#' @S3method getResponseTerms Formula
#' @author Matt Owen
getResponseTerms.Formula <- function (x, ..., single.only=FALSE, duplicates=TRUE)
{
  # Create and empty list
  list.formula <- list()

  # This loop goes through all the list response and predictor terms and 
  # creates a "Zelig-style" list based on it. This is so we can extract response
  # and predictor terms with "getResponstTerms" and "getPredictorTerms" in a
  # manageable way!
  for (resp in attr(x, "lhs")) {
    # Iterate through all response variables

    for (pred in attr(x, "rhs")) {
      # Iterate through all predictor variables

      # Append response variable and predictor terms
      # "ccc" is probably going to be convention for a call object in Zelig
      # models since "CALL", "call", "Call" all seem too similar to "call".
      # And we need to break
      ccc <- call("~", resp, pred)

      # Cast from a "call" object to a "formula" object
      ccc <- as.formula(ccc)

      # Append to list
      list.formula <- append(list.formula, ccc)

    }
  }
  
  # Important to send 'single.only'/'duplicates' into this function
  resp <- getResponseTerms(list.formula, ..., single.only, duplicates)

  # Apply unique only if 'duplicates' is FALSE
  # This ensures the list has the expected properties
  if (duplicates)
    resp

  else
    unique(resp)
}
