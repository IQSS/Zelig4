#' Print Values of a Summarized 'sim' Object
#' @S3method print summary.sim
#' @param x a 'summary.sim' object
#' @param ... ignored parameters
#' @return the value of the `summary.sim' object (invisibly)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.sim <- function(x, ...) {
  # Rename 'x' 'summary'
  summary <- x

  obj <- summary$zelig
  model <- summary$model
  x <- summary$x
  x1 <- summary$x1
  stats <- summary$stats
  num <- summary$num

  # Error if there are no statistics to display
  if (is.null(stats))
    stop("stats object cannot be NULL")

  # new-line
  cat("\n")

  # Print model name
  cat("Model: ", model, "\n")

  # Print number of simulations
  cat("Number of simulations: ", num, "\n")

  # new-line
  cat("\n")

  # Display information about the X setx object
  # This should probably be reconsidered in the future
  if (!is.null(x$matrix)) {
    cat("Values of X\n")
    print(as.matrix(x$updated))

    # new-line
    cat("\n")
  }
  else if (is.list(x$s.x)) {
    # add special hooks here?
  }

  # Display information about the X1 setx object
  # This should probably be reconsidered in the future
  if (!is.null(x1$matrix)) {
    cat("Values of X1\n")
    print(as.matrix(x1$updated))

    # new-line
    cat("\n")
  }

  # Decrementing the size of the list will give us an easy way to print
  size <- length(stats)

  # Loop across all qi's
  for (key in names(stats)) {
    # Create variable for code clarity
    val <- stats[[key]]

    if (!is.qi(val))
      next

    # Display Title
    cat(key, "\n")

    # Round value if numeric
    if (is.numeric(val))
      print(round(val*(1000))/1000)

    # Simply print if anything else
    else
      print(val)

    # Print a new-line between qi's
    if (size <- size - 1) {
      cat("\n")
    }
  }

  # Return invisibly
  invisible(x)
}
