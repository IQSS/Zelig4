qi.gamma <- function(z, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # get parameters
  shape <- gamma.shape(z)
  alpha <- rnorm(num, mean = shape$alpha, sd = shape$SE)
  coef <- coef(param)

  # compute eta
  eta <- coef %*% t(x)

  # or do this: get the inverse function
  #inverse <- z[["family", "linkinv"]]
  # theta <- matrix(inverse(eta), nrow = nrow(coef))

  # compute theta (apply inverse)
  theta <- matrix(1/eta, nrow = nrow(coef))

  ev <- theta
  pr <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))

  # default to not available
  ev1 <- pr1 <- fd <- NA

  # compute predicted values
  for (i in 1:nrow(ev))
    pr[i,] <- rgamma(
                     ncol(ev),
                     shape = alpha[i],
                     scale = theta[i,]/alpha[i]
                     )
  
  # if x1 is not NULL, run more simultations
  # ...

  if (!is.null(x1)) {

    # quantities of interest
    results <- qi(z, x1, num=num, param=param)

    # pass values over
    ev1 <- results[["Expected Values: E(Y|X)"]]
    pr1 <- results[["Predicted Values: Y|X"]]

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
