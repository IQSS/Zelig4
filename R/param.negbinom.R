param.negbinom <- function(z, num=1000, bootstrap=NULL) {
  list(
       simulations = mvrnorm(num, mu=coef(z), Sigma=vcov(z)),
       alpha = z[["theta"]],
       link = function (e) e
       )
}
