param.ls <- function(z, num, bootstrap=F) {
  mvrnorm(n=num, mu=coef(z), Sigma=vcov(z))
}
