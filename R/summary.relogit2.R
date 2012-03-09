summary.relogit2 <- function(object, ...) {

  res <- list()
  res$lower.estimate <- summary.relogit(object$lower.estimate)
  res$upper.estimate <- summary.relogit(object$upper.estimate)
  res$call <- object$call
  class(res) <- "summary.relogit2"
  return(res)
}












