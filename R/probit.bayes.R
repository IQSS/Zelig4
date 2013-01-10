#' @export
zelig2probit.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {

  loadDependencies("MCMCpack", "coda")

  if (missing(verbose))
    verbose <- round((mcmc + burnin)/10)

  list(
       .function = "MCMCprobit",
       .hook = "MCMChook",

       formula = formula,
       data   = data,
       burnin = burnin,
       mcmc   = mcmc,
       verbose= verbose,

       # Most parameters can be simply passed forward
       ...
       )
}

#' @S3method param probit.bayes
param.probit.bayes <- function(obj, num=1000, ...) {
  list(
       coef = coef(obj),
       fam  = binomial(link="probit")
       )
}

#' @S3method qi probit.bayes
qi.probit.bayes <- qi.logit.bayes

#' @S3method describe probit.bayes
describe.probit.bayes <- function(...) {
  list(
       description  = "Bayesian Probit Regression for Dichotomous Dependent Variables",
       authors = c("Ben Goodrich", "Ying Lu"),
       year = 2013
       )
}
