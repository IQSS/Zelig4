#' Summary for ``Relogit'' Fitted Model
#'
#' Summarize important components of the ``relogit'' model
#' @usage \method{summary}{Relogit}(object, ...)
#' @S3method summary Relogit
#' @param object a ``Relogit'' object
#' @param ... other parameters
#' @return a ``summary.relogit'' object
summary.Relogit <- function(object, ...) {
  dta <- model.matrix(terms(object), data=model.frame(object))
  class(object) <- class(object)[2]
  res <- summary(object, ...)
  if (object$bias.correct) {
    n <- nrow(dta)
    k <- ncol(dta)
    res$cov.unscaled <- res$cov.unscaled * (n/(n+k))^2
    res$cov.scaled <- res$cov.unscaled * res$dispersion
    res$coefficients[,2] <- sqrt(diag(res$cov.scaled))
    res$coefficients[,3] <- res$coefficients[,1] / res$coefficients[,2]
    res$coefficients[,4 ] <- 2*pt(-abs(res$coefficients[,3]), res$df.residual)
  }
  res$call <- object$call
  res$tau <- object$tau
  res$bias.correct <- object$bias.correct
  res$prior.correct <- object$prior.correct
  res$weighting <- object$weighting
  class(res) <- "summary.relogit"
  return(res)
}
