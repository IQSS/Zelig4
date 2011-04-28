#' Compute quantities of interest for 'negbinom' Zelig models
#' @usage \method{qi}{negbinom}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi negbinom
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.negbinom <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
  #
  coef <- coef(param)
  alpha <- alpha(param)

  # get inverse function
  inverse <- obj[["family", "linkinv"]]

  #
  eta <- coef %*% t(x)
  theta <- matrix(inverse(eta), nrow=nrow(coef))

  # ...
  ev <- theta
  pr <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))

  # default values
  ev1 <- pr1 <- fd <- NA

  #
  for (i in 1:ncol(ev))
    pr[,i] <- rnegbin(nrow(ev), mu = ev[i,], theta = alpha[i])


  if (!is.null(x1)) {

    # quantities of interest
    results <- qi(obj, x1, num=num)

    # pass values over
    ev1 <- results[["Expected Values: E(Y|X)"]]
    pr1 <- results[["Predicted Values: Y|X"]]

    # compute first differences
    fd <- ev1 - ev
  }

  #
  list("Expected Values: E(Y|X)" = ev,
       "Predicted Values: Y|X" = pr,
       "Expected Values (for X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X1" = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
