#' extract samples from a distribution in order to pass them to the qi function
#' (this is primarily a helper function for the \\model\\ model)
#' param obj a zelig object
#' param num an integer specifying the number of simulations to compute
#' return a list specifying link, link-inverse, random samples, and ancillary parameters
param.\\model\\ <- function(obj, num=1000) {
  list(
       coef = NULL,
       linkinv = NULL
       )
}
