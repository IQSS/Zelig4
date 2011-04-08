#' Method to describe a model to Zelig
#'
#' @param ... parameters which are typically ignored
#' @return a list to be processed by `as.description'
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe <- function(...)
  UseMethod("describe")
