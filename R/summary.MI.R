#' Summarry of Multiply Imputed Statistical Models
#' @S3method summary MI
#' @export
#' @param x a set of fitted statistical models
#' @param ... parameters to forward
#' @return a list of summaries
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.MI <- function(x, ...) {

  Map(summary, x$result, ...)

}
