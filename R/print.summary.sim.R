#' Print Values of a Summarized 'sim' Object
#' @S3method print summary.sim
#' @param x a 'summary.sim' object
#' @param ... ignored parameters
#' @return the value of the `summary.sim' object (invisibly)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.sim <- function(x, ...) {
  # Let's do it this way:
  obj <- x$zelig
  model <- x$name

  x <- x$x
  x1 <- x$x1

  stats <- x$stats


  message("Quit Expectedly")
  q()

  # prints typically have qi, and qi.names defined as part of the summary object
  if (is.null(x$stats))
    stop("stats object cannot be NULL")

  # new-line
  cat("\n")

  # print model name
  cat("Model: ", x$model, "\n")

  # print number of simulations
  cat("Number of simulations: ", x$iterations, "\n")

  # new-line
  cat("\n")

  # important value of x
  if (!is.null(x$x$matrix)) {
    cat("Values of X\n")
    print(as.matrix(x$x$updated[, x$x$explan]))

    # new-line
    cat("\n")
  }
  else if (is.list(x$x$s.x)) {
    # add special hooks here?
  }

  # important value of x1
  if (!is.null(x$x1$matrix)) {
    cat("Values of X1\n")

    print(as.matrix(x$x1$updated[, x$x1$explan]))

    # new-line
    cat("\n")
  }

  # new-line
  cat("\n")

  #
  i <- iter(x$stats)
  first.iter <- T

  repeat {
    item <- try(nextElem(i), silent=T)

    #
    if (inherits(item, "try-error"))
      break

    # for code clarity
    key <- item$key
    val <- item$value

    if (!is.qi(val))
      next

    # for proper whitespace formatting
    if (first.iter)
      first.iter <- F
    else
      cat("\n")

    # display the title, then the value
    cat(key, "\n")
    #print(as.name(key))

    if (is.numeric(val))
      print(round(val*(1000))/1000)
    else
      print(val)
  }

  # return invisibly
  invisible(x)
}
