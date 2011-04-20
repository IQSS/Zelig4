#' Print values of a summarized `sim' object
#'
#' @S3method print summary.sim
#'
#' @param obj a `summary.sim' object
#' @param ... ignored parameters
#' @return the value of the `summary.sim' object (invisibly)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.sim <- function(x, ...) {
  obj <- x

  # prints typically have qi, and qi.names defined as part of the summary object
  if (is.null(obj$stats))
    stop("stats object cannot be NULL")

  # new-line
  cat("\n")

  # print model name
  cat("Model: ", obj$model, "\n")

  # print number of simulations
  cat("Number of simulations: ", obj$iterations, "\n")

  # new-line
  cat("\n")

  # important value of x
  if (!is.null(obj$x$matrix)) {
    cat("Values of X\n")
    print(as.matrix(obj$x$updated[, obj$x$explan]))

    # new-line
    cat("\n")
  }
  else if (is.list(obj$x$s.x)) {
    # add special hooks here?
  }

  # important value of x1
  if (!is.null(obj$x1$matrix)) {
    cat("Values of X1\n")

    print(as.matrix(obj$x1$updated[, obj$x1$explan]))

    # new-line
    cat("\n")
  }

  # new-line
  cat("\n")

  #
  i <- iter(obj$stats)
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
  invisible(obj)
}
