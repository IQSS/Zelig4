#' Method for summarizing simulations of quantities of interest
#'
#' @S3method summary MI.sim
#'
#' @param object a `MI.sim' object
#' @param ... ignored parameters
#' @return a `summarized.MI.sim' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.sim <- function(object, ...) {
  s <- object
  res <- list(model    = s$name,
              stats    = s$stats,
              titles   = s$titles,
              original = s$result,
              call     = s$call,
              zeligcall= s$zcall,
              x        = s$x,
              x1       = s$x1,
              iterations = s$iterations
              )
  class(res) <- c(s$name, "summary.sim")
  res
}
