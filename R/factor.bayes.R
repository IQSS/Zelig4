#' Interface between the Zelig Model factor.bayes and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2factor.bayes <- function (
                                formula, 
                                factors = 2,
                                burnin = 1000, mcmc = 20000, 
                                verbose=0, 
                                ..., 
                                data
                                ) {
  if (missing(verbose))
    verbose <- round((mcmc + burnin)/10)

  if (factors < 2)
    stop("Number of factors needs to be at least 2")

  x <- as.matrix(model.response(model.frame(formula, data=data, na.action=NULL)))

  list(
       .function = "MCMCfactanal",
       .hook = "McmcHookFactor",

       formula = formula,
       x = x,
       burnin = burnin,
       mcmc   = mcmc,
       verbose= verbose,
       data   = data,

       ...
       )
}
#' Simulate Parameters for the Bayesian Logistic Regression
#'
#' This method simulates parameters for the Bayesian Logistic Regression
#' (logit.bayes).
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... ignored parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary
#' parameters
#' @export
param.factor.bayes <- function (...) {
}
#' Compute Quantities of Interest for the Zelig Model factor.bayes
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of interest
#'         with their simulations
#' @export
qi.factor.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  list(
       "Expected Value: E(Y|X)" = NA
       )
}
#' Describe the factor.bayes Zelig Model
#' @param ... ignored parameters
#' @return a list specifying author, title, etc. information
#' @export
describe.factor.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
