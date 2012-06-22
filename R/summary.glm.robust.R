#' Summary of Generalized Linear Model with Robust Error Estimates
#'
#' Returns summary of a glm model with robust error estimates. This only
#' slightly differs from how the standard GLM's behave.
#' @S3method \method{summary}{glm.robust}(object, ...)
#' @S3method summary glm.robust
#' @param object a ``glm.robust'' fitted model
#' @param ... parameters to pass to the standard ``summary.glm'' method
#' @return a object of type ``summary.glm.robust'' and ``summary.glm''
summary.glm.robust <- function(object, ...) {
  class(object) <- c("glm", "lm")
  res <- summary.glm(object, ...)
  if (is.null(object$robust)) {
    res$cov.unscaled <- covmat.unscaled <- vcovHAC(object)
    res$robust <- "vcovHAC"
  } else {
    fn <- object$robust$method
    res$robust <- object$robust$method
    object$robust$method <- NULL
    arg <- object$robust
    arg$x <- object
    res$cov.unscaled <- covmat.unscaled <- eval(do.call(fn, args=arg))
  }
  res$cov.scaled <- covmat <- covmat.unscaled*res$dispersion
  if (!is.null(res$correlation)) {
    dd <- sqrt(diag(res$cov.unscaled))
    res$correlation <- res$cov.unscaled/outer(dd, dd)
    dimnames(res$correlation) <- dimnames(res$cov.unscaled)
  }

  res$coefficients[,2] <- s.err <- sqrt(diag(covmat))
  res$coefficients[,3] <- tvalue <- coefficients(object)/s.err
  if (length(dimnames(res$coefficients)[[2]])>3) {
    if (dimnames(res$coefficients)[[2]][3]=="z value")
      res$coefficients[,4] <- 2 * pnorm(-abs(tvalue))
    else
      res$coefficients[,4] <- 2 * pt(-abs(tvalue), object$df.residual)
  }
  class(res) <- c("summary.glm.robust","summary.glm")
  return(res)
}
