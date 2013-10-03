#' Interface between the Zelig Model lognorm and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2lognorm <- function (formula, ..., robust = FALSE, cluster = NULL, data) {

  loadDependencies("survival")

  if (!(is.null(cluster) || robust))
    stop("If cluster is specified, then `robust` must be TRUE")

  # Add cluster term
  if (robust || !is.null(cluster))
    formula <- cluster.formula(formula, cluster)

  # Return
  list(
       .function = "survreg",
       formula = formula,
       dist = "lognormal",
       robust = robust,
       data = data,
       ...
       )
}

#' @S3method param lognorm
param.lognorm <- function(obj, num=1000, ...) {

  # These are the fitted parameters
  coef <- coef(obj)

  # Append the log-scale
  mu <- c(coef, log(obj$result$scale))

  # These are their correlations
  cov <- vcov(obj)

  # Simulate the results
  simulations <- mvrnorm(num, mu, cov)

  # Return
  list(
       coef = as.matrix(simulations[, 1:length(coef)]),
       alpha = as.matrix(simulations[, -(1:length(coef))]),
       linkinv = survreg.distributions[["lognormal"]]$itrans
       )
}

#' @S3method qi lognorm
qi.lognorm <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  linkinv <- linkinv(param)
  alpha <- alpha(param)
  beta <- coef(param)

  # Compute expected values for "lognorm" regression
  #
  # This function is nested within qi.lognorm for code-clarity and because it
  # will not be used by any other function
  # @param coef
  # @param alpha sim.scale
  # @param x
  # @return a matrix
  compute.ev <- function (coef, alpha, x) {
    if (is.null(x) || is.na(x))
      # If there are missing explanatory variables, ignore them
      return(NA)

    # Compute eta
    # This value must be *inverted* to be restored to the true "observed" value
    eta <- coef %*% t(x)

    # Apply inverse link function
    theta <- as.matrix(apply(eta, 2, linkinv))

    # Copied from qi.survreg in Zelig v3.5
    ev <- exp(log(theta) + 0.5*(exp(alpha))^2)
    dimnames(ev) <- dimnames(theta)

    # Return
    as.matrix(ev)
  }

  # Compute expected values for X and X1
  ev1 <- compute.ev(beta, alpha, x)
  ev2 <- compute.ev(beta, alpha, x1)


  list(
       "Expected Values: E(Y|X)" = ev1,
       "Expected Values: E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1
       )
}

#' @S3method describe lognorm
describe.lognorm <- function(...) {
  list(
       authors = c("Matthew Owen", "Olivia Lau", "Kosuke Imai", "Gary King"),
       text = "Log-Normal Regression for Duration Dependent Variables",
       year = 2007
       )
}
