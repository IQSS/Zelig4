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

  cat("Model name = ", model, "\n")
  cat("Formula    = ")
  print(formula)

  cat("\nComplete data.frame:\n")
  print(x$updated)

  cat("\nModel Matrix (Design Matrix):\n")
  print(x$matrix)

  invisible()
}
#' @S3method print summary.setx
print.summary.setx <- function (x, ...) {
  cat("\nModel name =", x$model.name, "\n")
  cat("Label      =", x$label, "\n")
  cat("Formula    = ")
  print(x$formula)

  cat("\nCall:\n")
  print(x$call)

  cat("\nModel Matrix (Design Matrix):\n")
  print(x$model.matrix)

  invisible(x)
}
#' Print values of `sim' objects
#' 
#' This function is currently unimplemented, and included for future development
#' @usage \method{print}{sim}(x, ...)
#' @S3method print sim
#' @param x a `sim' object (ignored)
#' @param ... ignored parameters
#' @return NULL (invisibly)
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.sim <- function(x, ...) {
  o <- x
  class(o) <- 'list'
  print(o)
}
#' Print a Summary MCMCZelig Object
#'
#' This method prints a summary object for \code{MCMCZelig} objects
#' @param x an "MCMCZelig" object
#' @param digits a numeric specifying the precision of the summary object
#' @param ... ignored parameters
#' @return a \code{summary.MCMCZelig} object
#' @S3method print summary.MCMCZelig
print.summary.MCMCZelig <- function(x, digits=max(3, getOption("digits") - 
3), ...) {
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
#' Print Multiply Imputed Simulations Summary
#'
#' Prints summary information about Multiply Imputed Fits
#' @usage \method{print}{summarySim.MI}(x, digits=3, ...)
#' @S3method print summarySim.MI
#' @param x a 'summarySim.MI' object
#' @param digits an integer specifying the number of digits of precision to
#'   print
#' @param ... ignored parameters
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summarySim.MI <- function(x, digits=3, ...) {
  for (qi.name in names(x)) {
    if (!is.valid.qi.list(x[[qi.name]]))
      next

    summed.qi <- qi.summarize(qi.name, x[[qi.name]])
    print(summed.qi)
    cat("\n")
  }

  invisible(x)
}

#' Row-bind Matrices and Lists
#' @param x a list or a matrix
#' @param y a list or a matrix
#' @return a matrix
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
.bind <- function (x, y) {

  # Get names for future columns

  if (!is.matrix(x))
    x <- matrix(x, nrow=1, ncol=length(x), dimnames=list(NULL, names(x)))

  if (missing(y))
    return(x)

  if (!is.matrix(y))
    y <- matrix(y, nrow=1, ncol=length(y), dimnames-list(NULL, names(y)))

  names <- unique(c(colnames(x), colnames(y)))

  ncol <- length(names)

  X <- matrix(NA, nrow=nrow(x), ncol=ncol, dimnames=list(NULL, names))
  Y <- matrix(NA, nrow=nrow(y), ncol=ncol, dimnames=list(NULL, names))

  X[, colnames(x)] <- x
  Y[, colnames(y)] <- y

  rbind(X, Y)
}

#' Check If Object Is a List of Valid Quantities of Interest
#' @param x an object to be tested
#' @return TRUE or FALSE
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
is.valid.qi.list <- function (x) {

  # if it is not a list or that list has no entries
  if (!(is.list(x) && length(x)))
    return(FALSE)

  # if any are not a matrix

  for (val in x) {

    if (is.matrix(val) && !(ncol(val) && ncol(val)))
      return(FALSE)

    else if (is.list(val) && !length(val))
      return(FALSE)
  }

  TRUE
}
#' Print values of ``zelig'' objects
#'
#' Print the zelig object as a list
#' @usage \method{print}{zelig}(x, ...)
#' @S3method print zelig
#' @param x a `zelig' object
#' @param ... ignored parameters
#' @return the `zelig' object (invisibly)
#' @export 
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.zelig <- function(x, ...) {
  class(x) <- "list"
  print(x)
}
