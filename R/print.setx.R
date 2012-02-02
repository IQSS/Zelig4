#' Print values of `setx' objects
#'
#' Print a ``setx'' object in human-readable form.
#' @usage \method{print}{setx}(x, ...)
#' @S3method print setx
#' @param x a `setx' object
#' @param ... ignored parameters
#' @return the value of x (invisibly)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.setx <- function(x, ...) {
  model <- x$name
  formula <- x$formula
  label <- x$label

  cat("Call:\n")
  print(x$call)

  cat("Label      =", label, "\n")
  cat("Model name =", model, "\n")
  cat("Formula    = ")
  print(formula)

  cat("\nComplete data.frame:\n")
  print(x$updated)

  invisible()
}
