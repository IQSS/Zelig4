#' Bundle Data-sets for Multiple Imputation
#' 
#' This object prepares data-sets for processing with multiple imputation.
#' @note This function is largely identical to simply creating a list object,
#'   with the exception that any unnamed data-sets are automatically labeled
#'   via the \code{substitute} function
#' @param ... a set of \code{data.frame}'s
#' @return an \code{almost.mi} object, which contains the important internals
#'   of a valid, useful \code{mi} object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
mi <- function (...) {

  # Get arguments as list
  data.frames <- list(...)

  # Ensure that everything is data.fram
  for (k in length(data.frames):1) {
    if (!is.data.frame(data.frames[[k]]))
      data.frames[[k]] <- NULL
  }

  # Return
  data.frames
}
