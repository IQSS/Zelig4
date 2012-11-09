#' @export
zelig2oprobit.bayes <- function (
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
       .function = "MCMCoprobit",
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
#' @S3method param oprobit.bayes
param.oprobit.bayes <- function(obj, num=1000, ...) {

  # Produce the model matrix in order to get all terms (explicit and implicit)
  # from the regression model.
  mat <- model.matrix(obj$result, data=obj$data)

  # Response Terms
  p <- ncol(mat)

  # All coefficients
  coefficients <- coef(obj)

  # Coefficients for predictor variables
  beta <- coefficients[, 1:p]

  # Middle values of "gamma" matrix
  mid.gamma <- coefficients[, -(1:p)]

  # ...
  level <- ncol(coefficients) - p + 2


  # Initialize the "gamma" parameters
  gamma <- matrix(NA, nrow(coefficients), level + 1)

  # The first, second and last values are fixed
  gamma[, 1] <- -Inf
  gamma[, 2] <- 0
  gamma[, ncol(gamma)] <- Inf

  # All others are determined by the coef-matrix (now stored in mid.gamma)
  if (ncol(gamma) > 3)
    gamma[, 3:(ncol(gamma)-1)] <- mid.gamma

  # return
  list(
       simulations = beta,
       alpha   = gamma,
       linkinv = NULL
       )
}
#' @S3method qi oprobit.bayes
qi.oprobit.bayes <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
{
  labels <- levels(model.response(model.frame(obj$result)))

  res1 <- compute.oprobit.bayes(x, param, labels)
  res2 <- compute.oprobit.bayes(x1, param, labels)

  # 
  list(
       "Expected Value: E(Y|X)" = res1$ev,
       "Predicted Value: Y|X"   = res1$pv,
       "Expected Value (for X1): E(Y|X1)" = res2$ev,
       "Predicted Value (for X1): Y|X1"   = res2$pv,
       "First Differences: E(Y|X1) - E(Y|X)" = res2$ev - res1$ev
       )
}
# Helper function used to generate expected values
compute.oprobit.bayes <- function (x, param, labels) {
  # If either of the parameters are invalid,
  # Then return NA for both qi's
  if (is.null(x) || is.na(x) || is.null(param))
    return(list(ev=NA, pv=NA))


  # Extract simulated parameters
  beta <- coef(param)
  gamma <- alpha(param)

  # x is implicitly cast into a matrix
  eta <- beta %*% t(x)

  # **TODO: Sort out sizes of matrices for these things.
  ev <- array(NA, c(nrow(eta), ncol(gamma) - 1, ncol(eta)))
  pv <- matrix(NA, nrow(eta), ncol(eta))

  # Compute Expected Values
  # ***********************
  # Note that the inverse link function is:
  #   pnorm(gamma[, j+1]-eta) - pnorm(gamma[, j]-eta)
  for (j in 1:(ncol(gamma)-1)) {
    ev[, j, ] <- pnorm(gamma[, j+1]-eta) - pnorm(gamma[, j]-eta)
  }

  colnames(ev) <- labels


  # Compute Predicted Values
  # ************************
  for (j in 1:nrow(pv)) {
    mu <- eta[j, ]
    pv[j, ] <- as.character(cut(mu, gamma[j, ], labels=labels))
  }


  # **TODO: Update summarize to work with at most 3-dimensional arrays
  ev <- ev[, , 1]


  # Return
  list(ev = ev, pv = pv)
}
#' @S3method describe oprobit.bayes
describe.oprobit.bayes <- function(...) {
  list(
       authors = "Skyler Cranmer",
       text = "Ordinal Probit Regression for Bayesian Models"
       )
}
