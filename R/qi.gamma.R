#' Compute quantities of interest for 'gamma' Zelig models
#' @usage \method{qi}{gamma}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi gamma
#' @param obj a \code{zelig} object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#' though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#' interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.gamma <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # Get parameters
  shape <- gamma.shape(.fitted)
  alpha <- rnorm(num, mean = shape$alpha, sd = shape$SE)
  coef <- coef(param)


  # Compute eta
  eta <- coef %*% t(x)

  # Compute theta (apply inverse)
  theta <- matrix(1/eta, nrow = nrow(coef))

  ev <- theta
  pr <- matrix(NA, nrow = nrow(theta), ncol = ncol(theta))

  # Default to not available
  ev1 <- pr1 <- fd <- NA

  # Compute predicted values
  for (i in 1:nrow(ev))
    pr[i,] <- rgamma(
                     ncol(ev),
                     shape = alpha[i],
                     scale = theta[i,]/alpha[i]
                     )

  # if x1 is not NULL, run more simultations
  # ...

  if (!is.null(x1)) {

    eta1 <- coef %*% t(x1)
    ev1 <- theta1 <- matrix(1/eta1, nrow = nrow(coef))
    pr1 <- matrix(NA, nrow = nrow(theta1), ncol = ncol(theta1))

    for (i in 1:nrow(ev1))
      pr1[i, ] <- rgamma(ncol(ev1), shape = alpha[i], scale = theta1[i,]/alpha[i])

    fd <- ev1 - ev
  }

  # Return
  list("Expected Values: E(Y|X)" = ev,
       "Expected Values (for X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values: Y|X1" = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
