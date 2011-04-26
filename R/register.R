#' Generic Method for Registering Methods to Zelig Objects
#' This function is used internally by Zelig to produce a list of methods to 
#' reproduce for particular Zelig models.
#' @note This function is used exlusively internally by Zelig
#' @param obj a 'zelig' object
#' @param ... typically ignored parameters
#' @return a character-vector specifying the names of generic methods that can
#'   be applied to the specified Zelig model
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
register <- function(obj, ...) {
  UseMethod("register")
}
