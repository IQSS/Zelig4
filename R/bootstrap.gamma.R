#' Bootstrap Parameters for Zelig ``gamma'' GLM
#'
#' Returns bootstrapped parameter estimates for a ``gamma'' GLM.
#' @S3method bootstrap gamma
#' @param obj a ``zelig'' object that will be used to produce boot-strapped
#' parameters
#' @param num an integer specifying the number of simulations to produce
#' @param ... extra parameters to be passed to the ``boot'' method. These are
#' typically ignored, but is included for further expansion.
#' @return a list containing information concerning link, link-inverses, etc.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.gamma <- function (obj, num=200, ...) {
  coef(obj)
}
