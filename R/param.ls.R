#' param method for the `ls' Zelig model
#'
#' @S3method param ls
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.ls <- function(z, num, bootstrap=F) {
  mvrnorm(n=num, mu=coef(z), Sigma=vcov(z))
}
