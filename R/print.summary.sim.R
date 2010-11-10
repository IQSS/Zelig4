print.summary.sim <- function(obj) {
  # prints typically have qi, and qi.names defined as part of the summary object
  if (is.null(obj$stats))
    stop("stats object cannot be NULL")

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
  else if (is.list(obj$x$s.x)) {
    # add special hooks here?
    # polymorphism for this stuff?
    #print(obj$x$s.x[[1]]$matrix)
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

    #
    if (is.na(val) || (is.list(val) && !length(val)) || is.null(val))
      next

    # for proper whitespace formatting
    if (first.iter)
      first.iter <- F
    else
      message()

    # display the title, then the value
    message(key)

    if (is.numeric(val))
      print(round(val*(1000))/1000)
    else
      print(val)
  }

  # return invisibly
  invisible(obj)
}
