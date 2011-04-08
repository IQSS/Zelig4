#' Generic Method for Simulating Ancillary/Auxillary Parameters of Zelig Models
#'
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... optional parameters which will likely be ignored
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param <- function (obj, num, ...)
  UseMethod("param")

#' Default method for param
#'
#' If no `param' function is set for a Zelig model, then
#' this function will return NULL.
#'
#' @S3method param default
#'
#' @param obj ignored parameter
#' @param num ignored parameter
#' @param ... ignored parameters
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.default <- function (obj, num, ...)
  NULL
