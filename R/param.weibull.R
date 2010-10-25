param.weibull <- function(z, num, ...) {
  # get object
  object <- GetObject(z)

  # get coefficients
  coef <- c(coef(object), log(object$scale))

  mvrnorm(n=num, mu=coef, Sigma=vcov(object))
}
