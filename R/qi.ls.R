qi.ls <- function(z, x=NULL, x1=NULL, num=1000, param=NULL, bootstrap=NULL) {
  # error-catching
  if (missing(x))
    stop("x cannot be missing while computing the `ls' model")

  # get `parameters'... whatever that means
  coefs <- coef(param)

  # compute expected value
  ev <- coefs %*% t(x)
  ev1 <- NA
  fd <- NA
  
  if (!is.null(x1)) {
    ev1 <- coefs %*% t(x1)
    fd <- ev1 - ev
  }

  # return
  list("Expected Value: E(Y|X)" = ev,
       "Expected Value (of X1): E(Y|X1)" = ev1,
       "First Difference in Expected Values: E(Y|X1) - E(Y|X)" = fd
       )
}
