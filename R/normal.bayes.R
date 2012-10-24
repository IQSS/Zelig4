#' Interface between the Zelig Model normal.bayes and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2normal.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose= 0, 
                               ..., 
                               data
                               ) {
  list(
       .function = "MCMCregress",
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

#' @S3method param normal.bayes
param.normal.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       linkinv = gaussian()
       )
}

#' @S3method qi normal.bayes
qi.normal.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
{

  res1 <- normal.ev(x, param)
  res2 <- normal.ev(x1, param)

  list(
       "Expected Value: E(Y|X)" = res1$ev,
       "Predicted Value: Y|X" = res1$pv,
       "Expected Value (for X1): E(Y|X1)" = res2$ev,
       "Predicted Value (for X1): Y|X1" = res2$pv,
       "First Differences: E(Y|X1) - E(Y|X)" = res2$ev - res1$ev
       )
}

normal.ev <- function (x, param) {
  # If either of the parameters are invalid,
  # Then return NA for both qi's
  if (is.null(x) || is.na(x) || is.null(param))
    return(list(ev=NA, pv=NA))

  # Extract simulated parameters and get column names
  coef <- coef(param)
  cols <- colnames(coef)

  # Place the simulated variances in their own vector
  sigma2 <- coef[, ncol(coef)]

  # Remove the "sigma2" (variance) parameter which should already be placed
  # in the simulated parameters
  cols <- cols[ ! "sigma2" == cols ]
  
  #
  coef <- coef[, cols]

  #
  ev <- coef %*% t(x)
  pv <- rnorm(nrow(ev), ev, sqrt(sigma2))

  #
  list(ev = ev, pv = pv)
}

#' @S3method describe normal.bayes
describe.normal.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
