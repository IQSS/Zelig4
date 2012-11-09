#' Print a Summary MCMCZelig Object
#'
#' This method prints a summary object for \code{MCMCZelig} objects
#' @param x an "MCMCZelig" object
#' @param digits a numeric specifying the precision of the summary object
#' @param ... ignored parameters
#' @return a \code{summary.MCMCZelig} object
#' @S3method print summary.MCMCZelig
#' @usage print summary.MCMCZelig
print.summary.MCMCZelig <- function(x, digits=max(3, getOption("digits") - 3), ...) {
  cat("\nCall: ") 
  print(x$call) 
  cat("\n", "Iterations = ", x$start, ":", x$end, "\n", sep = "")
  cat("Thinning interval =", x$thin, "\n")
  cat("Number of chains =", x$nchain, "\n")
  cat("Sample size per chain =", (x$end -
  x$start)/x$thin + 1, "\n")
  cat("\n", "Mean, standard deviation, and quantiles for marginal posterior distributions.", "\n")
  print(round(x$summary, digits=digits))
  cat("\n")
}
print.summary.glm.robust <-
    function (x, digits = max(3, getOption("digits") - 3),
	      symbolic.cor = x$symbolic.cor,
	      signif.stars = getOption("show.signif.stars"), ...)
{
  class(x) <- "summary.glm"
  print(x)
  cat("\nRobust standard errors computed using", x$robust)
  cat("\n")
  invisible(x)
}

#' Print a Summary of a Set of Pooled Simulated Interests
#'
#' Prints the summary information from a set of pooled simulated interests. This
#' method assumes that quantities of interest are kept in a data type which can
#' be used with ``rbind''.
#' @usage \method{print}{summary.pooled.sim}(x, ...)
#' @S3method print summary.pooled.sim
#' @param x a ``summary.pooled.sim'' object, containing summarized information
#' about simulated quantities of interest
#' @param ... Optional parameters that will be passed onward to ``print.matrix''
#' (the matrix printing function)
#' @return a ``summary.pooled.sim'' object storing the quantities of interest
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.pooled.sim <- function (x, ...) {
  # los labels... kinda like spanish for "the labels"
  # labels is function name in base, so we needed a name that said "labels,"
  # without using "labels". You know?
  los.labels <- x$labels
  los.titles <- x$titles

  # Pooled summarized data
  for (title in los.titles) {

    # This will implicity become a matrix
    m <- NULL

    for (label in los.labels)
      m <- rbind(m, x$stats[[label]][[title]])

    rownames(m) <- paste("[", los.labels, "]", sep="")

    cat(title, "\n")
    print(m)
    cat("\n\n")
  }
}
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
#' Print Values of a Summarized ``sim'' Object
#'
#' Print values of simulated quantities of interest (stored in a ``summary.sim''
#' object.
#' @usage \method{print}{summary.sim}(x, ...)
#' @S3method print summary.sim
#' @param x a 'summary.sim' object
#' @param ... ignored parameters
#' @return the value of the `summary.sim' object (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.sim <- function(x, ...) {
  # Rename 'x' 'summary'
  summary <- x

  obj <- summary$zeligcall
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
    print(as.matrix(x$matrix))

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
    print(as.matrix(x1$matrix))

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
