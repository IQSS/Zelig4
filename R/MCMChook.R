#' Hook to Clean-up MCMC Objects
#'
#' This method gives valid methods to the resulting MCMC object so that it can
#' be used with Zelig.
#' @note This function is used internally by the ZeligBayesian package.
#' @param obj the fitted model object (in this case a \code{mcmc} object.
#' @param model.call the call made to the external model
#' @param zelig.call the actual call to zelig itself
#' @param ... ignored parameters
#' @return an object useable by Zelig
#' @author Olivia Lau, Kosuke Imai, Gary King and Matt Owen
#' @export
MCMChook <- function (obj, model.call, zelig.call, seed=NULL, ...)
{

  # Create a new object
  res <- list()

  attr(obj, "call") <- NULL

  # Add the bare necessities for a zelig object
  res$coefficients <- obj
  res$formula <- zelig.call$formula
  res$data <- zelig.call$data
  res$model <- model.frame(eval(res$formula), data = eval(res$data))
  res$terms <- attr(res$model, "terms")
  res$call <- model.call

  # Ensure that a "seed" element exists
  res$seed <- if (is.null(seed))
    NA
  else
    seed

  class(res) <- "MCMCZelig"

  res
}

#' Hook to Clean-up MCMC Factor Object
#'
#' This method gives valid methods to the resulting MCMC object so that it can
#' be used with Zelig.
#' @note This function is used internally by the ZeligBayesian package.
#' @param obj the fitted model object (in this case a \code{mcmc} object.
#' @param model.call the call made to the external model
#' @param zelig.call the actual call to zelig itself
#' @param ... ignored parameters
#' @return an object useable by Zelig
#' @author Olivia Lau, Kosuke Imai, Gary King and Matt Owen
#' @export
McmcHookFactor <- function (obj, model.call, zelig.call, seed = NULL, ...) {

  out <- list()

  out$coefficients <- obj
  out$formula <- zelig.call$formula
  out$data <- zelig.call$data
  out$model <- model.frame(eval(out$formula), eval(out$data))
  out$terms <- attr(out$model, "terms")
  out$call <- model.call

  # Factors have no intercept term?
  attr(out$terms,"intercept") <- 0

  if (is.null(zelig.call$seed))
    out$seed <- NA
  else
    out$seed <- zelig.call$seed

  class(out) <- "MCMCZelig"

  out
}
