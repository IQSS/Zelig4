#' Matrix transpose of a setx object
#' @S3method t setx
#' @param x a `setx' object
#' @return a transposed matrix
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
t.setx <- function(x)
  t(x$matrix)
