#' Convert a 'setx' Object to a Matrix
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
#' @usage \method{as.matrix}{setx}(x, ...)
#' @param x a setx object
#' @param ... ignored parameters
#' @return a matrix containing columns and rows corrseponding to the explanatory
#' variables specified in the call to the 'setx' function
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.matrix.setx <- function(x, ...) {
  if (!is.null(x$matrix))
    #as.matrix(x$updated[, x$explan])
    x$matrix
  
  else
    stop("unspecified error")
}
