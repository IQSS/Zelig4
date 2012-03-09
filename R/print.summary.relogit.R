#' Print Summary of a Rare-event Logistic Model
#'
#' ...
#' @S3method print summary.relogit
#' @param x ...
#' @param digits ...
#' @return x (invisibly)
print.summary.relogit <- function(x,
                                  digits = max(3, getOption("digits") - 3),
                                  ...
                                  ) {
  class(x) <- "summary.glm"
  print(x, digits = digits, ...)
  if (x$prior.correct) 
    cat("\nPrior correction performed with tau =", x$tau, "\n")
  if (x$weighting) 
    cat("\nWeighting performed with tau =", x$tau, "\n")
  if (x$bias.correct)
    cat("Rare events bias correction performed\n")
  if (!is.null(x$robust))
    cat("Robust standard errors computed using", x$robust, "\n")

  message("?????????")
  message("?????????")
  message("?????????")
  message("?????????")
  message("?????????")
  invisible(x)  
}
