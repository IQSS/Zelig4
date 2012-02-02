#' Matrix Transpose of a ``setx'' Object
#'
#' Returns a ``setx'' object as column vector. If multiple values for each
#' explanatory term has been set, then return a NxM matrix where `N'
#' is the number of explanatory terms and `M' is the number of values set for
#' each term.
#'
#' @S3method t setx
#' @usage \method{t}{setx}(x)
#' @param x a `setx' object
#' @return a transposed matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
t.setx <- function(x)
  t(x$matrix)
