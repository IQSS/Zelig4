#' Print values of summary data for simulations of multiply-imputed data
#'
#' @S3method print summary.MI.sim
#'
#' @param x a `summary.MI.sim' object, which is the result of calling `summary'
#'          on a `sim' object
#' @param ... ignored parameters
#' @return the value of the `summary.MI.sim' object (invisibly)
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
print.summary.MI.sim <- function(x, ...) {
  obj <- x

  print(obj$stats)
  print(length(obj$stats))
  print(names(obj))
  print(obj$titles)


  # prints typically have qi, and qi.names defined as part of the summary object
  if (is.null(obj$qi.stat) || is.null(obj$qi.name)) {
    stop("qi.stat or qi.name cannot be NULL")
  }

  # warn if name lists do not match
  if (any(sort(names(obj$qi.stat)) != sort(names(obj$qi.name)))) {
    warning("quantities of interest do not match its name list")  
  }

  # new-line
  message()

  # print model name
  cat("Model: ")
  message(obj$model)

  # print number of simulations
  cat("Number of simulations: ")
  message(obj$iterations)

  # new-line
  message()

  # important value of x
  if (!is.null(obj$x$matrix)) {
    message("Values of X")
    print(obj$x$matrix)

    # new-line
    message()
  }

  # important value of x1
  if (!is.null(obj$x1$matrix)) {
    message("Values of X1")
    print(obj$x1$matrix)

    # new-line
    message()
  }

  # new-line
  message()
  
  for (key in names(obj$qi.stat)) {
    #
    s <- obj$qi.name[[key]]
    s <- gsub("\\s+$", "", s)

    #
    val <- obj$qi.stat[[key]]


    # if there are no complete cases, just skip it
    if (length(val[complete.cases(val)]) < 1)
      next

    # only pass along things without NA values
    val <- val[complete.cases(val),]
    
    # print title
    message(s)


    # print the matrix of qi's
    print(val)

    # new-line
    message()
  }

  invisible(obj)
}
