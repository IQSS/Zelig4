#' Describe the type of model that this Zelig
#' object used to fit the data-set
#'
#' @param obj a `zelig' object
#' @param ... ignored parameters
#' @return a list specified by describe. + `model name'
#'         of this fitted model
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.zelig <- function(zelig.obj, ...) {

  append(list(model=zelig.obj$name),
         NextMethod("describe")
         )
}
