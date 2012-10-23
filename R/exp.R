#' Interface between the Zelig Model exp and 
#' the Pre-existing Model-fitting Method
#' @param formula a formula
#' @param ... additonal parameters
#' @param robust a boolean specifying whether to use robust error estimates
#' @param cluster a vector describing the clustering of the data
#' @param data a data.frame 
#' @return a list specifying '.function'
#' @export
zelig2exp <- function (formula, ..., robust = FALSE, cluster = NULL, data) {

  if (!(is.null(cluster) || robust))
    stop("If cluster is specified, then `robust` must be TRUE")

  # Add cluster term
  if (robust || !is.null(cluster))
    formula <- cluster.formula(formula, cluster)

  # Return
  list(
       .function = "survreg",

       formula = formula,
       dist = "exponential",
       robust = robust,
       data = data,
       ...
       )
}


stratify.rqs <- function (obj) {
  x <- vector("list", length(obj$tau))

  for(i in 1:length(obj$tau)) {
    xi <- obj

    xi$coefficients <- xi$coefficients[, i]
    xi$residuals <- xi$residuals[, i]
    xi$tau <- xi$tau[i]
    class(xi) <- "rq"

    x[[i]] <- xi 
  }

  names(x) <- obj$tau
  x
}
#' Param Method for the \code{exp} Zelig Model
#' @note This method is used by the \code{param} Zelig model
#' @usage \method{param}{exp}(obj, num, ...)
#' @S3method param exp
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.exp <- function(obj, num=1000, ...) {
  cov <- vcov(.object)
  mu <- coef(.object)

  # Return
  list(
       coef = mvrnorm(num, mu=mu, Sigma=cov),
       linkinv = survreg.distributions[["exponential"]]$itrans
       )
}
#' Compute quantities of interest for 'exp' Zelig models
#' @usage \method{qi}{exp}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi exp
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.exp <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  linkinv <- linkinv(param)

  # Compute Expected Values for the "exp" Regression
  # @param simulations 
  # @param x
  # @return a matrix
  compute.ev <- function (simulations, x) {

    if (is.null(x) || is.na(x))
      # If there are missing explanatory variables, ignore them
      return(NA)

    # Compute eta, which is the "flattened" prediction.
    # This value must be *inverted* to be restored to the true "observed" value
    eta <- simulations %*% t(x)

    # Return as a matrix, since this should be a vector at this point.
    as.matrix(apply(eta, 2, linkinv))
  }


  # Compute Predicted Values
  compute.pv <- function (ev, param) {
    NA
  }


  # Compute expected values for X and X1
  ev1 <- compute.ev(coef(param), x)
  ev2 <- compute.ev(coef(param), x1)

  # Compute Predicted values for X and X1


  list(
       "Expected Value: E(Y|X)" = ev1,
       "Expected Value: E(Y|X1)" = ev2,
       "First Differences: E(Y|X1) - E(Y|X)" = ev2 - ev1
       )
}
#' Describe a ``exp'' model to Zelig
#' @usage \method{describe}{exp}(...)
#' @S3method describe exp
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
describe.exp <- function(...) {
  list(
       authors = "",
       text = ""
       )
}
