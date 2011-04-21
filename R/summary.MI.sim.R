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

  summarized.list <- list()

  for (key in names(object)) {

    stats <- object[[key]]$stats

    for (qi.name in names(stats))
      summarized.list[[qi.name]][[key]] <- stats[[qi.name]]

  }

  class(summarized.list) <- "summarySim.MI"

  summarized.list
}
