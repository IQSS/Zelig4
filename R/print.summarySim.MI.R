#' Print Multiply Imputed Simulations Summary
#'
#' Prints summary information about Multiply Imputed Fits
#' @usage \method{print}{summarySim.MI}(x, digits=3, ...)
#' @S3method print summarySim.MI
#' @param x a 'summarySim.MI' object
#' @param digits an integer specifying the number of digits of precision to
#'   print
#' @param ... ignored parameters
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summarySim.MI <- function(x, digits=3, ...) {
  for (qi.name in names(x)) {
    if (!is.valid.qi.list(x[[qi.name]]))
      next

    summed.qi <- qi.summarize(qi.name, x[[qi.name]])
    print(summed.qi)
    cat("\n")
  }

  invisible(x)
}

#' Row-bind Matrices and Lists
#' @param x a list or a matrix
#' @param y a list or a matrix
#' @return a matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.bind <- function (x, y) {

  # Get names for future columns

  if (!is.matrix(x))
    x <- matrix(x, nrow=1, ncol=length(x), dimnames=list(NULL, names(x)))

  if (missing(y))
    return(x)

  if (!is.matrix(y))
    y <- matrix(y, nrow=1, ncol=length(y), dimnames-list(NULL, names(y)))

  names <- unique(c(colnames(x), colnames(y)))

  ncol <- length(names)

  X <- matrix(NA, nrow=nrow(x), ncol=ncol, dimnames=list(NULL, names))
  Y <- matrix(NA, nrow=nrow(y), ncol=ncol, dimnames=list(NULL, names))

  X[, colnames(x)] <- x
  Y[, colnames(y)] <- y

  rbind(X, Y)
}

#' Check If Object Is a List of Valid Quantities of Interest
#' @param x an object to be tested
#' @return TRUE or FALSE
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
is.valid.qi.list <- function (x) {

  # if it is not a list or that list has no entries
  if (!(is.list(x) && length(x)))
    return(FALSE)

  # if any are not a matrix

  for (val in x) {

    if (is.matrix(val) && !(ncol(val) && ncol(val)))
      return(FALSE)

    else if (is.list(val) && !length(val))
      return(FALSE)
  }

  TRUE
}
