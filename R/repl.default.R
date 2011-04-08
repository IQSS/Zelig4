#' Default method for replicating data
#' @param obj a `zelig' object
#' @param data a data.frame
#' @param ... ignored parameters
#' @return a `sim' object storing the replicated quantities of interest
#' @export
#' @author Kosuke Imai and Olivia Lau \email{mowen@@iq.harvard.edu}
repl.default <- function(obj, data=NULL, ...) {

  #
  if (!is.null(data))
    obj$call$data <- data


  #
  eval(obj$call$data, sys.parent())

}
