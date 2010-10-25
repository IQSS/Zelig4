param.probit <- function(z, num=1000, bootstrap=NULL) {
  list(
       simulations = mvrnorm(n=num, mu=coef(z), Sigma=vcov(z)),
       alpha = NULL,
       fam = binomial(link="probit")
       )
}
