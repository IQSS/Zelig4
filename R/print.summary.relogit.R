#' Print Summary of a Rare-event Logistic Model
#'
#' Prints the 
#' @usage
#' \method{print}{summary.relogit}(x, digits = max(3, getOption("digits") - 3), ...)
#' @S3method print summary.relogit
#' @param x an ``relogit.summary'' object produced by the ``summary'' method.
#' @param digits an integer specifying the number of digits of precision to
#' specify
#' @param ... parameters passed forward to the ``print.glm'' function
#' @return x (invisibly)
print.summary.relogit <- function(
                                  x,
                                  digits = max(3, getOption("digits") - 3),
                                  ...
                                  ) {
  # Straight-forwardly print the model using glm's method
  print.glm(x, digits = digits, ...)

  #  Additional slots

  # Prior co
  if (x$prior.correct) 
    cat("\nPrior correction performed with tau =", x$tau, "\n")

  # Weighting? Sure, if it exists, we'll print it.
  if (x$weighting) 
    cat("\nWeighting performed with tau =", x$tau, "\n")

  # If there is bias-correction
  if (x$bias.correct)
    cat("Rare events bias correction performed\n")

  # If robust errors are computed...
  if (!is.null(x$robust))
    cat("\nRobust standard errors computed using", x$robust, "\n")

  # This is not a mutator assignment!
  class(x) <- "summary.glm"

  # Return object to be printed invisibly
  invisible(x)  
}
