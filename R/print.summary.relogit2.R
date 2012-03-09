#' Print Summary of a Rare-event Logistic Model
#'
#' ...
#' @S3method print summary.relogit2
#' @param x ...
#' @param digits ...
#' @return x (invisibly)
print.summary.relogit2 <- function(x, digits = max(3, getOption("digits") - 3),
                                  ...){
  cat("\nCall:\n", deparse(x$call), "\n\n", sep = "")
  print(x$lower.estimate)
  print(x$upper.estimate)
}
