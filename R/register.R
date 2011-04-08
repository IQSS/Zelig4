#' Generic method for registering methods
#'
#' @param obj a `zelig' object
#' @param ... typically ignored parameters
#' @return a character-vector specifying the names of 
#'         generic methods that can be applied to the
#'         specified Zelig model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
register <- function(obj, ...) {
  UseMethod("register")
}
