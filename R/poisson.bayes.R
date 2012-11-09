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

#' @S3method param poisson.bayes
param.poisson.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       fam = poisson()
       )
}

#' @S3method qi normal.bayes
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

#' @S3method describe poisson.bayes
describe.poisson.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
