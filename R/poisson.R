#' Interface between poisson model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2poisson <- function(formula, weights=NULL, ..., data) {
  z(
    glm,
    # .hook = "robust.glm.hook",
    formula = formula,
    weights = weights,
    family  = poisson(),
    model   = F,
    data    = data
    )
}
#' Param Method for the 'poisson' Zelig Model
#' @note This method is used by the 'poisson' Zelig model
#' @usage \method{param}{poisson}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.poisson <- function (obj, num=1000, ...) {
  list(
       simulations = mvrnorm(num, mu=coef(.fitted), Sigma=vcov(.fitted)),
       fam = poisson()
       )
}
#' Compute quantities of interest for 'poisson' Zelig models
#' @usage \method{qi}{poisson}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi poisson
#' @param obj a 'zelig' object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.poisson <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {
  # 
  coef <- coef(param)

  # get inverse function
  inverse <- linkinv(param)

  #
  eta <- coef %*% t(x)
  theta <- matrix(inverse(eta), nrow=nrow(coef))

  # ...
  ev <- theta
  pr <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))

  # default values
  ev1 <- pr1 <- fd <- NA

  for (i in 1:ncol(ev))
    pr[,i] <- rpois(nrow(ev), lambda = ev[,i])


  if (!is.null(x1)) {

    # quantities of interest
    results <- qi(obj, x1, num=num, param=param)

    # pass values over
    ev1 <- results[["Expected Values: E(Y|X)"]]
    pr1 <- results[["Predicted Values: Y|X"]]

    # compute first differences
    fd <- ev1 - ev
  }

  #
  list("Expected Values: E(Y|X)" = ev,
       "Expected Values (for X1): E(Y|X1)" = ev1,
       "Predicted Values: Y|X" = pr,
       "Predicted Values: Y|X1" = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
#' Describe the `poisson' model to Zelig
#' @usage \method{describe}{poisson}(...)
#' @S3method describe poisson
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.poisson <- function(...) {
  # parameters object
  parameters <- list(lambda = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.vars = TRUE,
                       exp.vars = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "count",
       parameters = parameters,
       text = "Poisson Regression for Event Count Dependent Variables"
       )
}
