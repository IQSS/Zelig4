#' param method for the `normal' Zelig model
#'
#' @S3method param normal
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.normal <- function(z, num=1000, bootstrap=NULL) {
  degrees.freedom <- z[["df.residual"]]
  sig2 <- summary(z$result)$dispersion

  list(
       simulations = mvrnorm(n=num, mu=coef(z), Sigma=vcov(z)),
       alpha = sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom)),
       link = function (x) x,
       linkinv = function (x) x
       )
}
