qi.logit <- function(z, x=NULL, x1=NULL, num=1000, param=NULL) {
  #
  coef <- coef(param)
  link.inverse <- linkinv(param)

  # computations
  eta <- coef %*% t(x)
  theta <- matrix(link.inverse(eta), nrow = nrow(coef))

  
  # ...
  ev <- theta
  pr <- matrix(nrow=nrow(theta), ncol=ncol(theta))

  
  # init
  ev2 <- pr2 <- fd <- NA

  
  # fill?
  for (i in 1:ncol(theta))
    pr[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i]))


  #
  if (!is.null(x1)) {
    eta2 <- coef %*% t(x1)
    theta2 <- matrix(link.inverse(eta2), nrow = nrow(coef))

    # ...
    ev2 <- theta2
    pr2 <- matrix(nrow=nrow(theta2), ncol=ncol(theta2))

    for (i in 1:ncol(theta))
      pr2[,i] <- as.character(rbinom(length(ev[,i]), 1, ev[,i]))

    fd <- ev2-ev
  }


  # return
  list("Expected Values: E(Y|X)" = ev,
       "Predicted Values: Y|X" = pr,
       "Expected Values (for X1)" = ev2,
       "Predicted Values (for X1" = pr2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
