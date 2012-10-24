#' Interface between the Zelig Model normal.bayes and 
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
zelig2poisson.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {
  if (missing(verbose))
    verbose <- round((mcmc + burnin)/10)

  list(
       .function = "MCMCpoisson",
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
#' (this is primarily a helper function for the poisson.bayes model)
#' @param obj a zelig object
#' @param num an integer specifying the number of simulations to compute
#' @param ... additional parameters
#' @return a list specifying link, link-inverse, random samples, and ancillary parameters
#' @export
param.poisson.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       fam = poisson()
       )
}
#' Compute Quantities of Interest for the Zelig Model normal.bayes
#' @S3method qi normal.bayes
#' @param obj a zelig object
#' @param x a setx object
#' @param x1 an optional setx object
#' @param y ...
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
qi.poisson.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
{

  res1 <- poisson.ev(x, param)
  res2 <- poisson.ev(x1, param)

  list(
       "Expected Value: E(Y|X)" = res1$ev,
       "Predicted Value: Y|X" = res1$pv,
       "Expected Value (for X1): E(Y|X1)" = res2$ev,
       "Predicted Value (for X1): Y|X1" = res2$pv,
       "First Differences: E(Y|X1) - E(Y|X)" = res2$ev - res1$ev
       )
}

poisson.ev <- function (x, param) {
  # If either of the parameters are invalid,
  # Then return NA for both qi's
  if (is.null(x) || is.na(x) || is.null(param))
    return(list(ev=NA, pv=NA))

  # Extract inverse-link and simulated parameters (respectively)
  inv <- linkinv(param)
  eta <- coef(param) %*% t(x)

  # Give matrix identical rows/columns to the simulated parameters
  ev <- pv <- matrix(NA, nrow(eta), ncol(eta))
  dimnames(ev) <- dimnames(pv) <- dimnames(eta)

  # Compute Expected Values
  ev <- inv(eta)

  # Compute Predicted Values
  for (i in 1:ncol(ev))
    pv[, i] <- rpois(length(ev[, i]), ev[, i])

  list(ev=ev, pv=pv)
}
#' Describe the poisson.bayes Zelig Model
#' @param ... ignored parameters
#' @return a list specifying author, title, etc. information
#' @export
describe.poisson.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
