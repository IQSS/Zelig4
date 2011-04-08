#' Extract the fitted model object from the Zelig object
#'
#' @param obj an object of type `zelig'
#' @return the fitted model object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
GetObject <- function(obj) {
  if (inherits(obj, 'zelig'))
    obj$result
}
