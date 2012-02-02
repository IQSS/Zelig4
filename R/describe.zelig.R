#' Get Description Object Used to Cite this Zelig Model
#' @note This function should be reevaluated in design, since 'description'
#' objects are exclusively used internally. In particular, this method would
#' be more useful to users as a 'cite' method.
#' @usage \method{describe}{zelig}(object, ...)
#' @S3method describe zelig
#' @param object a 'zelig' object
#' @param ... ignored parameters
#' @return a 'description' object used internally to produce citation text
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.zelig <- function(object, ...) {
  append(list(model=object$name), NextMethod("describe"))
}
