#' Convert a ``pooled.setx'' Object to a Matrix
#'
#' The setx object is, in its most basic form, a list of column names and values
#' specified for each of these column names. This function simply converts the
#' key-value pairs of column-name and specified value into a matrix.
#'
#' @note This method allows basic matrix arithmetic operations on data objects,
#' which mirror values stored within setx objects. In many scenarios,
#' simulations require matrix-multiplication, etc. to be performed on a
#' data-set. This function faciliates that need.
#' 
#' @usage \method{as.matrix}{pooled.setx}(x, ...)
#' @S3method as.matrix pooled.setx
#' @param x a setx object
#' @param ... ignored parameters
#' @return a matrix containing columns and rows corrseponding to the explanatory
#' variables specified in the call to the 'setx' function
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.matrix.pooled.setx <- function(x, ...) {
  big.matrix <- NULL
  for (label in names(x)) {
    small.matrix <- as.matrix(x[[label]])
    big.matrix <- rbind(big.matrix, small.matrix)
  }

  rownames(big.matrix) <- names(x)
  attr(big.matrix, "labels") <- names(x)
  attr(big.matrix, "which") <- 1:nrow(big.matrix)
  names(attr(big.matrix, "which")) <- names(x)

  big.matrix
}
