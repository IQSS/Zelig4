#' Interface between the Zelig Model logit.bayes and 
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
zelig2logit.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {

  loadDependencies(MCMCpack)

  if (missing(verbose))
    verbose <- round((mcmc + burnin)/10)

  list(
       .function = "MCMClogit",
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


#' @S3method param logit.bayes
param.logit.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       fam  = binomial(link="logit")
       )
}

#' @S3method qi logit.bayes
qi.logit.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # Use a Helper-Function that computes expected values and predicted values
  # simultaneously.
  res1 <- logit.ev(x, param)
  res2 <- logit.ev(x1, param)

  # Return quantities of interest
  list(
       "Expected Value: E(Y|X)" = res1$ev,
       "Predicted Value: Y|X" = res1$pv,
       "Expected Value (for X1): E(Y|X1)" = res2$ev,
       "Predicted Value (for X1): Y|X1" = res2$pv,
       "First Differences: E(Y|X1)-E(Y|X)" = res2$ev - res1$ev
       )
}

logit.ev <- function (x, param) {
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
    pv[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i])) 

  # Return
  list(ev=ev, pv=pv)
}

#' @S3method describe logit.bayes
describe.logit.bayes <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
