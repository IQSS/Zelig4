#' Interface between gamma model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2gamma <- function(formula, ..., data)
  z(
    glm,
    # .hook = "robust.glm.hook",

    formula = formula,
    family  = Gamma(),
    model   = F,
    data    = data
    )
#' param method for the `gamma' Zelig model
#'
#' Return parameter estimates for the ``gamma'' GLM in Zelig.
#' @usage \method{param}{gamma}(obj, num, ...)
#' @S3method param gamma
#' @param obj a `zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a `parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.gamma <- function(obj, num = 1000, ...) {
  # Extract shape parameters, which will be used to simulate the ancillary
  # parameters
  shape <- gamma.shape(.object)

  # Simulate ancillary parameters
  alpha <- rnorm(n=num, mean=shape$alpha, sd=shape$SE)

  #
  list(
       simulations  = mvrnorm(n=num, mu=coef(.object), Sigma=vcov(.object)),
       alpha = alpha,
       family = Gamma()
       )
}
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
  list("Expected Values: E(Y|X)"  = ev,
       "Expected Values: E(Y|X1)" = ev1,
       "Predicted Values: Y|X"    = pr,
       "Predicted Values: Y|X1"   = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
#' Describe the \code{gamma} model to Zelig
#' @usage \method{describe}{gamma}(...)
#' @S3method describe default
#' @param ... ignored parameters
#' @return a list of important information
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.gamma <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "bounded",
       parameters = parameters,
       text = "Gamma Regression for Continuous, Positive Dependent Variables"
       )
}
