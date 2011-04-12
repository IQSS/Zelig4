#' Extract Samples from a Distribution in Order to Pass Them to the `qi' Function
#' (this is primarily a helper function for the \\model\\ model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
#' @export
param.\\model\\ <- function(obj, num=1000) {
  list(
       coef = NULL,
       linkinv = NULL
       )
}
