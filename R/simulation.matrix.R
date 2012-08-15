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
  if (missing(which))
    stop('The "which" parameter is unspecified')

  else if (!any(which %in% names(obj$qi))) {
    warning(
      'The "which" parameter does not exist. Valid titles are:\n    ',
      paste('"', names(obj$qi), '"', sep="", collapse=", ")
      )

    # Return a matrix containing the single entry NA
    return(matrix(NA))
  }

  # Return the result as a matrix (it should be either a matrix or vector...)
  as.matrix(obj$qi[[which]])
}

#' @S3method simulation.matrix pooled.sim
simulation.matrix.pooled.sim <- function (obj, which, ...) {

  # This will become the matrix that is returned
  big.matrix <- NULL

  # Iterate through all the results
  for (label in names(obj)) {
    # Get the matrix for the single quantity of interest
    small.matrix <- simulation.matrix(obj[[label]], which = which)

    # Column-bind this result with the total matrix.
    # This might want to be wrapped by a tryCatch in case weird things happen
    big.matrix <- cbind(big.matrix, small.matrix)
  }

  # Column-wise specification
  attr(big.matrix, "labels") <- names(obj)
  attr(big.matrix, "which") <- 1:ncol(big.matrix)
  names(attr(big.matrix, "which")) <- names(obj)

  # Return the big matrix
  big.matrix
}
