param.logit <- function(z, num, bootstrap=FALSE) {
  list(
       simulations = mvrnorm(n=num, mu=coef(z), Sigma=vcov(z)),
       alpha       = NULL,
       fam = binomial(link="logit")
       )
}
