#' Bootstrap Parameters for Zelig ``normal'' GLM
#'
#' Returns bootstrapped parameter estimates for a Gaussian GLM.
#' @S3method bootstrap normal
#' @param obj a ``zelig'' object that will be used to produce boot-strapped
#' parameters
#' @param num an integer specifying the number of simulations to produce
#' @param ... extra parameters to be passed to the ``boot'' method. These are
#' typically ignored, but is included for further expansion.
#' @return a list containing information concerning link, link-inverses, etc.
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
bootstrap.normal <- function (obj, ...) {
  degrees.freedom <- obj[["df.residual"]]
  sig2 <- summary(obj$result)$dispersion
  alpha <- sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom))

  list(
       alpha = alpha,
       beta = coef(obj)
       )
}
