#' Casts a `setx' object as a matrix
#'
#' @param x a setx object
#' @param ... ignored parameters
#' @return a matrix containing columns and rows corrseponding
#'         to the explanatory variables specified in the call
#'         to the `setx' function
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.matrix.setx <- function(x, ...) {
  if (!is.null(x$matrix))
    #as.matrix(x$updated[, x$explan])
    x$matrix
  
  else
    stop("unspecified error")
}
