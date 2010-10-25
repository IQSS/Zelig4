param.normal <- function(z, num=1000, bootstrap=NULL) {
  degrees.freedom <- z[["df.residual"]]
  sig2 <- summary(z$result)$dispersion

  list(
       simulations = mvrnorm(n=num, mu=coef(z), Sigma=vcov(z)),
       alpha = sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom)),
       link = function (x) x,
       linkinv = function (x) x
       )
}
