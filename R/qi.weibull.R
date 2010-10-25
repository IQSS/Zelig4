qi.weibull <- function(z, x=NULL, x1=NULL, num=1000, param=NULL) {
  if (is.null(x))
    stop("???")

  # prepare x matrices
  x.matrix <- as.matrix(x)


  if (!is.null(x1))
    x1.matrix <- as.matrix(x1)


  # get parameters
  parameters <- param(z, num=num)

  #
  len <- length(coef(z))

  #
  sim.coef <- parameters[,1:len]
  sim.scale <- parameters[,-(1:len)]

  # robust check?
  # ...


  # survival module is kind of ghetto...
  # don't know how i feel about this line of code.
  link <- survreg.distributions[[ "weibull" ]]$itrans

  # i prefer this.



  # prepare matrices for robust
  if(z[["call", "robust"]]) {
    x.matrix <- x.matrix[,-ncol(x.matrix), drop=F]

    if (!is.null(x1))
      x1.matrix <- x1.matrix[,ncol(x1.matrix), drop=F]
  }

  #
  eta <- sim.coef %*% t(x.matrix)
  theta <- as.matrix(apply(eta, 2, link))

  #
  ev <- theta * gamma(1 + exp(sim.scale))
  pr <- matrix(NA, ncol=ncol(ev), nrow=nrow(ev))
  ev1 <- pr1 <- fd <- NA


  for (i in 1:nrow(pr)) {
    pr[i,] <- rweibull(length(ev[i,]),
                       shape=1/exp(sim.scale[i]),
                       scale=theta[i,]
                       )
  }


  if (!is.null(x1)) {
    qi.list <- qi(z, x=x1, num=num)
    ev1 <- qi.list[["Expected Values: E(Y|X)"]]
    pr1 <- qi.list[["Predicted Values: Y|X"]]
    fd <- ev1 - ev
  }
  
  list("Expected Values: E(Y|X)" = ev,
       "Expected Values: E(Y|X1)" = ev1,
       "Predicted VAlues: Y|X" = pr,
       "Predicted VAlues: Y|X1" = pr1,
       "First DIfference: E(Y|X1) - E(Y|X)" = fd
       )
       
}
