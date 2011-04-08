#' Generic method for replicating data from `zelig' objects
#' @param obj a `zelig' object
#' @param x a `setx' object
#' @param x1 a secondary `setx' object used to perform
#'           particular computations of quantities of interest
#' @param y a parameter reserved for the computation of particular
#'          quantities of interest (average treatment effects). Few
#'          models currently support this parameter
#' @param num an integer specifying the number of simulations to compute
#' @return a `sim' object storing the replicated quantities of interest
#' @export
#' @author Kosuke Imai and Olivia Lau \email{mowen@@iq.harvard.edu}
repl <- function(obj, ...)
  UseMethod("repl")
