#' Interface between normal model and Zelig
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2normal <- function(formula, weights=NULL, ..., data)
  z(
    glm,
    # .hook = "robust.glm.hook",
    formula = formula,
    weights = weights,
    family  = gaussian,
    model   = F,
    data    = data
    )
#' Param Method for the 'normal' Zelig Model
#' @note This method is used by the 'normal' Zelig model
#' @usage \method{param}{normal}(obj, num=1000, ...)
#' @S3method param negbinom
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.normal <- function(obj, num=1000, ...) {
  degrees.freedom <- .fitted$df.residual
  sig2 <- summary(.fitted)$dispersion

  list(
       simulations = mvrnorm(n=num, mu=coef(.fitted), Sigma=vcov(.fitted)),
       alpha = sqrt(degrees.freedom * sig2 / rchisq(num, degrees.freedom)),
       link = function (x) x,
       linkinv = function (x) x
       )
}
#' Compute quantities of interest for 'normal' Zelig models
#' @usage \method{qi}{normal}(obj, x, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi normal
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
qi.normal <- function(obj, x, x1=NULL, y=NULL, num=1000, param=NULL) {
  # get `num` samples from the underlying distribution
  coef <- coef(param)
  alpha <- alpha(param)

  # theta = eta, because inverse of 
  # normal models' link function is
  # the identity
  theta <- matrix(coef %*% t(x), nrow=nrow(coef))

  #
  pr <- matrix(NA, nrow=nrow(theta), ncol=ncol(theta))

  #
  ev <- theta
  ev1 <- pr1 <- fd <- NA
  
  for (i in 1:nrow(ev))
    pr[i,] <- rnorm(ncol(ev), mean = ev[i,], sd = alpha[i])


  # if x1 is not NULL, run more simultations
  # ...

  if (!is.null(x1)) {

    # quantities of interest
    lis1 <- qi(obj, x1, num=num, param=param)

    # pass values over
    ev1 <- lis1[[1]]
    pr1 <- lis1[[3]]

    # compute first differences
    fd <- ev1 - ev
  }

  # return
  list("Expected Values: E(Y|X)"  = ev,
       "Expected Values: E(Y|X1)" = ev1,
       "Predicted Values: Y|X"    = pr,
       "Predicted Values: Y|X1"   = pr1,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}
#' Describe the \code{normal} model to Zelig
#' @usage \method{describe}{normal}(...)
#' @S3method describe normal
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.normal <- function(...) {
  # parameters object
  parameters <- list(pi = list(
                       equations = c(1, 1),
                       tags.allowed = FALSE,
                       dep.var = TRUE,
                       exp.var = TRUE
                       )
                     )

  # return list
  list(authors  = c("Kosuke Imai", "Gary King", "Olivia Lau"),
       year     = 2008,
       category = "continuous",
       parameters = parameters,
       text = "Normal Regression for Continuous Dependent Variables"
       )
}
