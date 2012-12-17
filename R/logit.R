#' Interface between logit model and Zelig
#'
#' This function is exclusively for use by the `zelig' function
#' @param formula a formula
#' @param weights a numeric vector
#' @param robust a boolean (logical) specifying whether robust error estimates
#' should be used
#' @param ... ignored parameters
#' @param data a data.frame
#' @return a list to be coerced into a zelig.call object
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
zelig2logit <- function(formula, weights=NULL, robust = F, ..., data) {
  w <- weights
  z(
    glm,
    formula = formula,
    weights = w,
    family  = binomial(link="logit"),
    model   = F,
    data    = data
    )
}

#' Param Method for the \code{logit} Zelig Model
#' @note This method is used by the \code{logit} Zelig model
#' @usage \method{param}{logit}(obj, num, ...)
#' @S3method param logit
#' @param obj a 'zelig' object
#' @param num an integer specifying the number of simulations to sample
#' @param ... ignored parameters
#' @return a list to be cast as a 'parameters' object
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
param.logit <- function(obj, num, ...) {
  list(
       simulations = mvrnorm(n=num, mu=coef(.object), Sigma=vcov(.object)),
       alpha       = NULL,
       fam = binomial(link="logit")
       )
}

#' Compute quantities of interest for 'logit' Zelig models
#' @usage \method{qi}{logit}(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL)
#' @S3method qi logit
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
qi.logit <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  # Compute expected values
  compute.ev <- function(obj, x=NULL, num=1000, param=NULL) {
    if (is.null(x))
      return(NA)

    coef <- coef(param)
    link.inverse <- linkinv(param)

    eta <- coef %*% t(x)
    eta <- Filter(function (y) !is.na(y), eta)

    theta <- matrix(link.inverse(eta), nrow = nrow(coef))

    ev <- matrix(link.inverse(eta), ncol=ncol(theta))

    ev
  }

  # Simulate quantities of interest for "x"
  ev1 <- compute.ev(obj, x, num, param)
  pr1 <- matrix(nrow=nrow(ev1), ncol=ncol(ev1))

  # Simulate the quantities of interest for "x1"
  ev2 <- compute.ev(obj, x1, num, param)
  pr2 <- fd <- NA

  
  # Produce 0 or 1 (FALSE/TRUE) results for "x"
  for (i in 1:ncol(ev1))
    pr1[,i] <- as.character(rbinom(length(ev1[,i]), 1, ev1[,i]))

  # Produce 0 or 1 (FALSE/TRUE) results for "x1" and comppute first-differences
  if (!is.null(x1)) {
    pr2 <- matrix(nrow=nrow(ev2), ncol=ncol(ev2))

    for (i in 1:ncol(ev2))
      pr2[,i] <- as.character(rbinom(length(ev2[,i]), 1, ev2[,i]))

    # This is the computation of the first difference...
    fd <- ev2 - ev1
  }

  # Ensure that the correct levels are passed along.
  levels(pr1) <- levels(pr2) <- c('0', '1')

  # return
  list("Expected Values: E(Y|X)"  = ev1,
       "Expected Values: E(Y|X1)" = ev2,
       "Predicted Values: Y|X"    = pr1,
       "Predicted Values: Y|X1"   = pr2,
       "First Differences: E(Y|X1) - E(Y|X)" = fd
       )
}

.compute.ev <- function(obj, x=NULL, num=1000, param=NULL) {

  if (is.null(x))
    return(NA)

  coef <- coef(param)
  link.inverse <- linkinv(param)

  eta <- coef %*% t(x)

  theta <- matrix(link.inverse(eta), nrow = nrow(coef))

  ev <- matrix(link.inverse(eta), ncol=ncol(theta))

  ev
}

#' Describe a `logit' model to Zelig
#' @usage \method{describe}{logit}(...)
#' @S3method describe logit
#' @param ... ignored parameters
#' @return a list to be processed by `as.description'
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
#' @export
describe.logit <- function(...) {
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
       category = "dichotomous",
       parameters = parameters,
       text = "Logistic Regression for Dichotomous Dependent Variables"
       )
}
