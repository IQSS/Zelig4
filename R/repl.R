#' Generic Method for Replicating Data
#' @param object a 'zelig' object
#' @param ... parameters
#' @return a replicated object
#' @export
#' @author Kosuke Imai and Olivia Lau \email{mowen@@iq.harvard.edu}
repl <- function(object, ...)
  UseMethod("repl")
