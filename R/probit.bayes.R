#' Interface between the Zelig Model probit.bayes and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param burnin a parameter corresponding to the 'burnin' paramater for the
#' MCMCprobit function
#' @param mcmc a parameter corresponding to the 'mcmc' paramater for the
#' MCMCprobit function
#' @param verbose a parameter corresponding to the 'verbose' paramater for the
#' MCMCprobit function
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2probit.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {
  if (missing(verbose))
    verbose <- round((mcmc + burnin)/10)

  list(
       .function = "MCMCprobit",
       .hook = "MCMChook",

       formula = formula,
       data   = data,
       burnin = burnin,
       mcmc   = mcmc,
       verbose= verbose,

       # Most parameters can be simply passed forward
       ...
       )
}
#' Extract Samples from a Distribution in Order to Pass Them to the \code{qi} Function
#' (this is primarily a helper function for the probit.bayes model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
#' @export
param.probit.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       fam  = binomial(link="probit")
       )
}
#' Compute Quantities of Interest for the Zelig Model probit.bayes
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
#' @export
qi.probit.bayes <- qi.logit.bayes
#' Describe the probit.bayes Zelig Model
#' @param ... ignored parameters
#' @return a list specifying author, title, etc. information
#' @export
describe.probit.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
