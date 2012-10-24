#' Summary of MCMCZelig Object
#'
#' This method produces a summary object for \code{MCMCZelig} objects
#' @param object an "MCMCZelig" object
#' @param quantiles a numeric vector specifying the quantiles to use in the
#' summary object.
#' @return a \code{summary.MCMCZelig} object
#' @S3method summary MCMCZelig
summary.MCMCZelig <- function(object, quantiles = c(0.025, 0.5, 0.975), ...) {
  out <- list()
  out$summary <- cbind(
                       summary(coef(object))$statistics[,1:2],
                       summary(coef(object), quantiles=quantiles)$quantiles
                       )
                       
  colnames(out$summary) <- c("Mean", "SD", paste(quantiles*100, "%",sep=""))
  stuff <- attributes(coef(object))
  out$call <- object$call
  out$start <- stuff$mcpar[1]
  out$end <- stuff$mcpar[2]
  out$thin <- stuff$mcpar[3]
  out$nchain <- 1
  class(out) <- "summary.MCMCZelig"
  out
}

