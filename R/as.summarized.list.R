#' list -> summarized.qi
#' @param x a list
#' @param ... ignored parameters
#' @return a 'summarized.qi' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
as.summarized.list <- function(x, ...) {
  class(x) <- "summarized.qi"
  x
}
