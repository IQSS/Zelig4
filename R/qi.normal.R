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
