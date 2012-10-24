#' Interface between the Zelig Model probit.bayes and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param burnin a parameter corresponding to the 'burnin' paramater for the
#' MCMCprobit function
#' @param mcmc a parameter corresponding to the 'mcmc' paramater for the
#' MCMCprobit function
#' @param verbose a parameter corresponding to the 'verbose' paramater for the
#' MCMCprobit function
#' @param ... additonal parameters
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2probit.bayes <- function (
                               formula, 
                               burnin = 1000, mcmc = 10000, 
                               verbose=0, 
                               ..., 
                               data
                               ) {
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
       authors = "",
       text = ""
       )
}
