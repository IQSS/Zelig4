#' Construct a list of available generic methods available for the
#' object used to fit the Zelig model
#'
#' @S3method register default
#'
#' @param obj a `zelig' object
#' @return a character vector containing names of S3 methods for the statistical model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
register.default <- function(obj) {
  .GetGenerics(obj)
}
