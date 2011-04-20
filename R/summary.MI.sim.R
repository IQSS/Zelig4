#' Method for summarizing simulations of multiply imputed quantities of interest
#'
#' @S3method summary MI.sim
#'
#' @param object a `MI.sim' object
#' @param ... ignored parameters
#' @return a `summarized.MI.sim' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.MI.sim <- function(object, ...) {
  res <- NextMethod()
  class(res) <- c(object$name, "summary.MI.sim", "summary.sim")
  res
}
