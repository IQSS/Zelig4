#' Generic Method for Computing Quantities of Interest
#'
#' @param obj a `zelig' object
#' @param x a `setx' object or NULL
#' @param x1 an optional `setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'          though this feature is currentlysupported by only a
#'          handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of
#'         quantities of interest with their simulations
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi <- function(obj, x=NULL, x1=NULL, y=NULL, num, param=NULL) {
  # error-catching
  if (!inherits(obj, "zelig"))
    stop("z must be of type \"zelig\"")

  if (!(is.null(x) || inherits(x, "setx")))
    stop("x must be of type \"setx\"")

  if (!(is.null(x1) || inherits(x1, "setx")))
    stop("x1 must be of type \"setx\"")

  # then use the method
  UseMethod("qi")
}
