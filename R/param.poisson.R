#' param method for the `poisson' Zelig model
#'
#' @S3method param poisson
#' 
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @return a list to be cast as a `parameters' object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.poisson <- function (z, num=1000, bootstrap=NULL) {
  list(
       simulations = mvrnorm(num, mu=coef(z), Sigma=vcov(z)),
       fam = poisson()
       )
}
