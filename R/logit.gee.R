#' General Estimating Equation for Logit Regression
#' @param formula a formula
#' @param id a character-string specifying the column of the data-set to use
#' for clustering
#' @param robust a logical specifying whether to robustly or naively compute
#' the covariance matrix. This parameter is ignore in the \code{zelig2}
#' method, and instead used in the \code{robust.hook} function, which
#' executes after the call to the \code{gee} function
#' @param ... ignored parameters
#' @param R a square-matrix specifying the correlation
#' @param corstr a character-string specifying the correlation structure
#' @param data a data.frame 
#' @return a list specifying the call to the external model
#' @export zelig2logit.gee
#' @name logit.gee
#' @aliases zelig2logit.gee
zelig2logit.gee <- function (formula, id, robust, ..., R = NULL, corstr = "independence", data) {

  loadDependencies("gee")

  if (corstr == "fixed" && is.null(R))
    stop("R must be defined")

  # if id is a valid column-name in data, then we just need to extract the
  # column and re-order the data.frame and cluster information
  if (is.character(id) && length(id) == 1 && id %in% colnames(data)) {
    id <- data[, id]
    data <- data[order(id), ]
    id <- sort(id)
  }

  z(
    .function = gee,
    .hook = robust.gee.hook,

    formula = formula,
    id = id,
    corstr = corstr,
    family  = binomial(link="logit"),
    data = data,
    R = R,
    ...
    )
}


#' @S3method param logit.gee
param.logit.gee <- function(obj, num=1000, ...) {
  # Extract means to compute maximum likelihood
  mu <- coef(obj)

  # Extract covariance matrix to compute maximum likelihood
  Sigma <- vcov(obj)

  list(
       coef = mvrnorm(num, mu, Sigma),
       fam = binomial(link="logit")
       )
}


#' @S3method qi logit.gee
qi.logit.gee <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  coef <- coef(param)
  inverse <- linkinv(param)

  eta1 <- coef %*% t(x)
  ev1 <- theta1 <- matrix(inverse(eta1), nrow=num)

  # default to NA
  rr <- ev2 <- fd <- NA

  if (!is.null(x1)) {
    eta2 <- coef %*% t(x1)
    ev2 <- theta1 <- matrix(inverse(eta2), nrow=num)

    fd <- ev2 - ev1
    rr <- ev2/ev1
  }

  list(
       "Expected Values (for x): E(Y|X)"   = ev1,
       "Expected Values (for x1): E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd,
       "Risk Ratios: E(Y|X1)/E(Y|X)" = rr
       )
}


#' @S3method describe logit.gee
describe.logit.gee <- function(...) {
  list(
       authors = "Patrick Lam",
       text = "General Estimating Equation for Logistic Regression",
       year = 2011
       )
}
