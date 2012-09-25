#' Bootstrap Parameters for Zelig ``negbinom'' GLM
#'
#' Returns bootstrapped parameter estimates for a negative-binomial GLM.
#' @usage \method{bootstrap}{negbinom}(obj, ...)
#' @S3method bootstrap negbinom
#' @param obj a ``zelig'' object that will be used to produce boot-strapped
#' parameters
#' @param ... extra parameters to be passed to the ``boot'' method. These are
#' typically ignored, but is included for further expansion.
#' @return a list containing information concerning link, link-inverses, etc.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.negbinom <- function (obj, ...) {
  list(
       alpha = .fitted$theta,
       beta = coef(.fitted)
       )
}
