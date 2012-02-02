#' Summarry of Multiply Imputed Statistical Models
#'
#' ...
#' @S3method summary MI
#' @usage \method{summary}{MI}(object, ...)
#' @param object a set of fitted statistical models
#' @param ... parameters to forward
#' @return a list of summaries
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
summary.MI <- function(object, ...) {
  results <- list()
  for (key in names(object))
    results[[key]] <- summary(object[[key]]$result)
  results
}
