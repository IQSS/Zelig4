#' Interface between ls model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2ls <- function(formula, ..., data, weights=NULL)
  z(
    lm,
    formula = formula,
    weights = weights,
    model   = F,
    data    = data
    )
#' Param Method for the 'ls' Zelig Model
#' @note This method currently returns via a deprectated style
#' @usage \method{param}{ls}(obj, num, \dots)
#' @S3method param ls
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.ls <- function(obj, num, ...) {
  mvrnorm(n=num, mu=coef(.object), Sigma=vcov(.object))
}
#' Compute quantities of interest for 'ls' Zelig models
#' @usage \method{qi}{ls}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi ls
#' @param obj a \code{zelig} object
#' @param x a 'setx' object or NULL
#' @param x1 an optional 'setx' object
#' @param y this parameter is reserved for simulating average treatment effects,
#'   though this feature is currentlysupported by only a handful of models
#' @param num an integer specifying the number of simulations to compute
#' @param param a parameters object
#' @return a list of key-value pairs specifying pairing titles of quantities of
#'   interest with their simulations
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
qi.ls <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # error-catching
  if (missing(x))
    stop("x cannot be missing while computing the `ls' model")

  # Get coefficients of the linear model
  coefs <- coef(param)

  # compute expected value
  ev <- coefs %*% t(x)
  ev1 <- NA
  fd <- NA
  
  if (!is.null(x1)) {
    ev1 <- coefs %*% t(x1)
    fd <- ev1 - ev
  }

  # return
  list("Expected Values: E(Y|X)"  = ev,
       "Expected Values: E(Y|X1)" = ev1,
       "Predicted Values: Y|X"    = ev,
       "Predicted Values: Y|X1"   = ev1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
#' Describe a \code{ls} model to Zelig
#' @note \code{ls} stands for "least squares fit"
#' @usage \method{describe}{ls}(...)
#' @S3method describe ls
#' @param ... ignored parameters
#' @return a list to be processed by \code{as.description}
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.ls <- function(...){
  parameters <-list(mu = list(
                      equations = c(1,1),
                      tags.allowed = FALSE,
                      dep.vars = TRUE,
                      exp.vars = TRUE
                      )
                    )
  
  # return
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2007,
       category = "continuous",
       parameters = parameters,
       text = "Least Squares Regression for Continuous Dependent Variables"
       )
}
