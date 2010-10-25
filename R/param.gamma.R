param.gamma <- function(zelig.obj, num, bootstrap=F) {
  # shape
  shape <- gamma.shape(zelig.obj)

  alpha <- rnorm(n=num, mean=shape$alpha, sd=shape$SE)

  #
  list(
       coef  = mvrnorm(n=num, mu=coef(zelig.obj), Sigma=vcov(zelig.obj)),
       alpha = alpha,
       link  = function (x) 1/x,
       linkinv= function (x) 1/x
       )

}
