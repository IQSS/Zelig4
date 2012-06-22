#' Print Summary of a Rare-event Logistic Model
#'
#' ...
#' @usage
#' \method{print}{summary.relogit2}(x, digits = max(3, getOption("digits") - 3), ...)
#' @S3method print summary.relogit2
#' @param x the object to print
#' @param digits an integer specifying the number of digits of precision
#' @param ... ignored parameters
#' @return x (invisibly)
print.summary.relogit2 <- function(x,
                                   digits = max(3, getOption("digits") - 3),
                                  ...
                                  ) {
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  print(x$lower.estimate)
  print(x$upper.estimate)
}
