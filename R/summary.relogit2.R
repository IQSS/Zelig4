#' Summary for ``Relogit2'' Fitted Model
#'
#' Summarize important components of the ``relogit'' model
#' @S3method summary Relogit2
#' @param object a ``Relogit2'' object
#' @param ... other parameters
#' @return a ``summary.relogit2'' object
summary.Relogit2 <- function(object, ...) {
  res <- list()
  res$lower.estimate <- summary.Relogit(object$lower.estimate)
  res$upper.estimate <- summary.Relogit(object$upper.estimate)
  res$call <- object$call
  class(res) <- "summary.relogit2"
  return(res)
}












