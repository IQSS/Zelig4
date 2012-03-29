#' Bootstrap Parameters for Zelig ``negbinom'' GLM
#'
#' Returns bootstrapped parameter estimates for a negative-binomial GLM.
#' @S3method bootstrap negbinom
#' @param obj a ``zelig'' object that will be used to produce boot-strapped
#' parameters
#' @param ... extra parameters to be passed to the ``boot'' method. These are
#' typically ignored, but is included for further expansion.
#' @return a list containing information concerning link, link-inverses, etc.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.negbinom <- function (obj, ...) {
  print(obj$theta)
  q()
  list(
       alpha = obj$theta,
       beta = coef(obj)
       )
}
