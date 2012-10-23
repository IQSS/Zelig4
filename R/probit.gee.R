#' Interface between the Zelig Model probit.gee and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param id a character-string specifying the column of the data-set to use
#'   for clustering
#' @param robust a logical specifying whether to robustly or naively compute
#'   the covariance matrix. This parameter is ignore in the \code{zelig2}
#'   method, and instead used in the \code{robust.hook} function, which
#'   executes after the call to the \code{gee} function
#' @param ... ignored parameters
#' @param R a square-matrix specifying the correlation
#' @param corstr a character-string specifying the correlation structure
#' @param data a data.frame 
#' @return a list specifying the call to the external model
#' @export
zelig2probit.gee <- function (formula, id, robust, ..., R, corstr = "independence", data) {

  Zelig:::loadDependencies(gee)

  if (corstr == "fixed" && is.null(R))
    stop("R must be defined")

  # if id is a valid column-name in data, then we just need to extract the
  # column and re-order the data.frame and cluster information
  if (is.character(id) && length(id) == 1 && id %in% colnames(data)) {
    id <- data[, id]
    data <- data[order(id), ]
    id <- sort(id)
  }

  list(
       .function = "gee",
       .hook = "robust.hook",

       formula = formula,
       id = id,
       corstr = corstr,
       family  = binomial(link="probit"),
       data = data,
       ...
       )
}

#' @S3method param probit.gee
param.probit.gee <- param.gamma.gee <- function(obj, num=1000, ...) {

  # Extract means to compute maximum likelihood
  mu <- coef(obj)

  # Extract covariance matrix to compute maximum likelihood
  Sigma <- vcov(obj)

  #
  list(
       coef = mvrnorm(num, mu, Sigma),
       fam = binomial(link="probit")
       )
}

#' @S3method qi probit.gee
qi.probit.gee <- qi.logit.gee

#' @S3method describe probit.gee
describe.probit.gee <- function(...) {
  list(
       authors = "Patrick Lam",
       text = "General Estimating Equation for Poisson Regression",
       year = 2011
       )
}
