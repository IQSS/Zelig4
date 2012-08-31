#' Get Simulations as a Matrix
#'
#' Returns a MxN matrix where N is the number of simulations and M is the number
#' of predicted values. Additionally, a ``labels'' attribute is attached that
#' produces a human-readable identifier for each column.
#' @param obj an object, typically a ``sim'' or ``pooled.sim'' object.
#' @param which a character-vector specifying the \emph{titles} of quantities of
#' interest to extract
#' @param ... additional parameters
#' @return a simulation matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
simulation.matrix <- function (obj, which = NULL, ...) {
  UseMethod("simulation.matrix")
}

#' @S3method simulation.matrix sim
simulation.matrix.sim <- function (obj, which, ...) {

  which <- find.match(which, attr(obj, "titles"))

  if (is.na(which)) {
    warning(
      'The "which" parameter does not exist. Valid titles are:\n    ',
      paste('"', names(obj$qi), '"', sep="", collapse=", ")
      )

    # Return a matrix containing the single entry NA
    return(matrix(NA))
  }

  # Store the little matrix (probably a column-vector)
  lil.matrix <- as.matrix(obj$qi[[which]])

  # Specify what quantities of interest this matrix represents
  attr(lil.matrix, "qi") <- which

  # Return the little, modified matrix
  lil.matrix
}

#' @S3method simulation.matrix pooled.sim
simulation.matrix.pooled.sim <- function (obj, which, ...) {

  # Get the best match for the value "which"
  which <- find.match(which, attr(obj, "titles"))

  # This will become the matrix that is returned
  big.matrix <- NULL

  # Iterate through all the results
  for (label in names(obj)) {
    # Get the matrix for the single quantity of interest
    small.matrix <- simulation.matrix(obj[[label]], which = which, exact.match = FALSE)

    # Column-bind this result with the total matrix.
    # This might want to be wrapped by a tryCatch in case weird things happen
    big.matrix <- cbind(big.matrix, small.matrix)
  }

  # Column-wise specification
  attr(big.matrix, "labels") <- names(obj)
  attr(big.matrix, "which") <- 1:ncol(big.matrix)
  names(attr(big.matrix, "which")) <- names(obj)

  # Specify what quantities of interest this matrix represents
  attr(big.matrix, "qi") <- which

  # Return the big matrix
  big.matrix
}

#' Find a Partial or Exact Match from a Vector of Strings
#' Searches a vector of character-string, and returns the best match.
#' @param needle a character-string to search for in the 
#' @param haystack a vector of character-strings
#' @param
#' @return the best-matched string or NA
#' @details ``find.match'' attempts to use several common matching functions in
#' an order that sequentially prefers less strict matching, until a suitable
#' match is found. If none is found, then return the value of the ``fail''
#' parameter (defaults to NA). The functions used for matching are: ``match'',
#' ``charmatch'', and finally ``grep''.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
find.match <- function (needle, haystack, fail = NA) {

  # Having multiple approximate hits is bad form, since the string "x" can match
  # "xe", "xen", "xs", etc. If it allows this possibility, we'll be constructing
  # matrices out of potentially disparate quantities of interest. That is, it
  # obviously would not be good to match the string "Average" with 
  # "Averge Treatment Effect" and "Average Value".
  # That is, we want our matrices to be constructed consistently
  if (length(needle) != 1)
    return(NA)

  # Search the strings all at once for code clarity. We can write this smoother,
  # but then it sacrifices readability for nested if clauses.
  exact.match <- match(needle, haystack, nomatch = 0)
  partial.match <- charmatch(needle, haystack, nomatch = 0)
  grep.match <- grep(needle, haystack)[1]

  # If we found an exact match, then we go with it.
  if (exact.match != 0)
    return(haystack[exact.match])

  # If there is a unique partial match, then that will work too.
  else if (partial.match != 0)
    return(haystack[partial.match])

  # If there are non-unique partial matches, then we take the first incidence
  else if (!is.na(grep.match))
    return(haystack[grep.match])

  # If nothing else is good, then return whatever value a failure should be. NA by default
  return(fail)
}
