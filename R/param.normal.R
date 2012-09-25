#' Param Method for the 'normal' Zelig Model
#' @note This method is used by the 'normal' Zelig model
#' @usage \method{param}{normal}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.normal <- function(obj, num=1000, ...) {
  degrees.freedom <- .fitted$df.residual
  sig2 <- summary(.fitted)$dispersion

  list(
       simulations = mvrnorm(n=num, mu=coef(.fitted), Sigma=vcov(.fitted)),
       alpha = sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom)),
       link = function (x) x,
       linkinv = function (x) x
       )
}
