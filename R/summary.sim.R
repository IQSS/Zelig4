#' Method for summarizing simulations of quantities of interest
#'
#' Return a ``summary.sim'' object (typically for display)
#' @S3method summary sim
#' @usage \method{summary}{sim}(object, ...)
#' @param object a 'MI.sim' object
#' @param ... ignored parameters
#' @return a 'summarized.MI.sim' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.sim <- function(object, ...) {
  res <- list(
              model    = object$model,
              stats    = object$stats,
              titles   = object$titles,
              original = object$result,
              call     = object$call,
              zeligcall= object$zcall,
              x        = object$x,
              x1       = object$x1,
              num      = object$num
              )
  class(res) <- c(object$name, "summary.sim")
  res
}
