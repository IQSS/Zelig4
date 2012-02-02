#' list -> summarized.qi
#' Convert a list into a ``summarized.qi'' object
#' @usage \method{as.summarized}{list}(x, ...)
#' @param x a list
#' @param ... ignored parameters
#' @return a ``summarized.qi'' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized.list <- function(x, ...) {
  class(x) <- "summarized.qi"
  x
}
