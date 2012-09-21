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
