#' Print a Bundle of Data-sets
#'
#' @S3method print setx.mi
#' @usage \method{print}{setx.mi}(x, ...)
#' @param x a \code{setx} object to print
#' @param ... ignored parameters
#' @return the \code{setx} object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.setx.mi <- function(x, ...) {
  # Store size for readability
  size <- length(x)

  for (k in 1:size) {
    # Print object
    print(x[[k]])

    # If this is not the last element, print a new-line
    if (k < size)
      cat("\n")
  }

  invisible(x)
}
