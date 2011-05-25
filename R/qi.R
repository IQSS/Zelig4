#' The \code{qi} function is used by developers to simulated quantities of
#' interest. This method, as a result, is the most significant method of any
#' Zelig statistical model.
#'
#' @title Generic Method for Computing Quantities of Interest
#' @param obj a \code{zelig} object
#' @param x a \code{setx} object or NULL
#' @param x1 an optional \code{setx} object
#' @param y this parameter is reserved for simulating average treatment effects,
#'          though this feature is currentlysupported by only a
#'          handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of
#'         quantities of interest with their simulations
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @note Run \code{example(qi)} to see a trivial version of 
#' @examples
#' qi.some.model <- function(obj, x=NULL, x1=NULL, y=NULL, param=NULL) {
#'   list(
#'        "Expected Values: E(Y|X)" = NA,
#'        "Predicted Values: Y|X"   = NA
#'        )
#' }
qi <- function(obj, x=NULL, x1=NULL, y=NULL, num, param=NULL) {
  if (!inherits(obj, "zelig"))
    stop('"obj" must be of a "zelig" object')

  if (!(is.null(x) || inherits(x, "setx")))
    stop('"x" must be a "setx" object"')

  if (!(is.null(x1) || inherits(x1, "setx")))
    stop('"x1" must be a "setx" object')

  # then use the method
  UseMethod("qi")
}
