#' Matrix transpose of a setx object
#' @S3method setx object
#' @param x a `setx' object
#' @return: a transposed matrix
t.setx <- function(x)
  t(x$matrix)
