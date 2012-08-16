#' Method for plotting simulations
#'
#' Plot simulated quantities of interest.
#' @usage \method{plot}{sim}(x, xlab = "", ...)
#' @S3method plot sim
#' @param x a `sim' object
#' @param ... parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @return nothing
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.sim <- function (x, ...) {

  env <- tryCatch(
    asNamespace(x$package.name),
    # 
    error = function (e) { 
      warning("")
      globalenv()
    }
    )

  # If plotPackageName
  if (exists("plot.simulations", envir = env, mode="function")) {
    # Get the simulation, because we know it exists
    .plotter <- get("plot.simulations", envir = env, mode="function")

    # Pass to a temporary variable to improve the visibility of the traceback
    # if there is an error
    res <- .plotter(x, ...)

    # Return object (whatever it is)
    invisible(res)
  }

  # Otherwise we just use this fall-back


  1
}


#' Plot Any Simulation from the Zelig Core Package
#' Plots any simulation from the core package. In general, this function can
#' \emph{neatly} plot simulations containing five of the popular ``quantities
#' of interest'' - ``Expected Values: E(Y|X)'', ``Predicted Values: Y|X'',
#' ``Expected Values (for X1): E(Y|X1)'', ``Predicted Values (for X1): Y|X1''
#' and ``First Differences: E(Y|X1) - E(Y|X)''.
#' @param x an object
#' @param .... parameters passed to the ``plot'' and ``barplot'' functions
#' @return the original graphical parameters
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.simulations <- function (x, ...) {
  # Save old state
  old.par <- par(no.readonly=T)

  #
  ev.titles <- c('Expected Values: E(Y|X)', 'Expected Values (for X1): E(Y|X1)')
  pv.titles <- c('Predicted Values: Y|X', 'Predicted Values (for X1): Y|X1')

  if (is.null(x$x)) {
    return(par(old.par))
  }
  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1:2, 2, 1)
    color.x <- 'gray'

    # The plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }
  else {

    panels <- matrix(c(1:5, 5), ncol=2, nrow=3, byrow = TRUE)

    color.x <- rgb(242, 122, 94, maxColorValue=255)
    color.x1 <- rgb(100, 149, 237, maxColorValue=255)

    # This mixes the above two colors, and converts the result into hexadecimal
    color.mixed <- col2rgb(red=round((col2rgb(color.x) + col2rgb(color.x1))/2), maxColorValue=255)

    # Determine whether two "Expected Values" qi's exist
    both.ev.exist <- all(ev.titles %in% names(qi))
    # Determine whether two "Predicted Values" qi's exist
    both.pv.exist <- all(pv.titles %in% names(qi))

    panels <- if (xor(both.ev.exist, both.pv.exist))
      rbind(panels, c(6, 6))
    else if (both.ev.exist && both.pv.exist)
      rbind(panels, c(6, 7))

    # the plotting device:
    #
    # +-----------+    +-----------+
    # |  1  |  2  |    |  1  |  2  |
    # +-----+-----+    +-----+-----+
    # |  3  |  4  |    |  3  |  4  |
    # +-----+-----+ OR +-----+-----+
    # |     5     |    |     5     |
    # +-----------+    +-----------+
    # |  6  |  7  |    |     6     |
    # +-----+-----+    +-----+-----+
  }

  #
  layout(panels)

  titles <- list(
    pv = "Predicted Values: Y|X",
    pv1 = "Predicted Values (for X1): Y|X1",
    ev = "Expected Values: E(Y|X)",
    ev1 = "Expected Values: E(Y|X1)",
    fd - "First Differences: E(Y|X1) - E(Y|X)"
    )

  simulations.plot(qi[[titles$pv]], main = titles$pv, col = color.x, line.col = "black")
  simulations.plot(qi[[titles$ev]], main = titles$ev, col = color.x, line.col = "black")
  simulations.plot(qi[[titles$pv1]], main = titles$pv1, col = color.x1, line.col = "black")
  simulations.plot(qi[[titles$ev1]], main = titles$ev1, col = color.x1, line.col = "black")
  simulations.plot(qi[[titles$fd]], main = titles$fd, col = color.mixed, line.col = "black")

  simulations.plot(
    qi[["Predicted Values: Y|X"]],
    qi[["Predicted Values (for X1): Y|X1"]],
    main = "Comparison of Y|X and Y|X1",
    col = c(color.x, color.x1),
    line.col = "black")

  simulations.plot(
    qi[["Expected Values: E(Y|X)"]],
    qi[["Expected Values (for X1): E(Y|X1)"]],
    main = "Comparison of E(Y|X) and E(Y|X1)",
    col = c(color.x, color.x1),
    line.col = "black")



  # Restore old state
  par(old.par)

  # Return old parameter invisibly
  invisible(old.par)
}
