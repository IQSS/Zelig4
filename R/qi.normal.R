#' Compute quantities of interest for 'normal' Zelig models
#' @usage \method{qi}{normal}(z, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi normal
#' @param z a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.normal <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # get `num` samples from the underlying distribution
  coef <- coef(param)
  alpha <- alpha(param)

  # theta = eta, because inverse of 
  # normal models' link function is
  # the identity
  theta <- matrix(coef %*% t(x), nrow=nrow(coef))

  #
  pr <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))

  #
  ev <- theta
  ev1 <- pr1 <- fd <- NA
  
  for (i in 1:nrow(ev))
    pr[i,] <- rnorm(ncol(ev), mean = ev[i,], sd = alpha[i])


  # if x1 is not NULL, run more simultations
  # ...

  if (!is.null(x1)) {

    # quantities of interest
    lis1 <- qi(z, x1, num=num, param=param)

    # pass values over
    ev1 <- lis1[[1]]
    pr1 <- lis1[[3]]

    # compute first differences
    fd <- ev1 - ev
  }

  # return
  list("Expected Values: E(Y|X)" = ev,
       "Expected Values (for X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values: Y|X1" = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
