#' Method for plotting simulations
#'
#' Plot simulated quantities of interest.
#' @usage \method{plot}{sim}(x, xlab = "", ...)
#' @S3method plot sim
#' @param x a `sim' object
#' @param xlab labels for the x-axis
#' @param ... parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @return the current graphical parameters. This is subject to change in future
#' implementations of Zelig
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.sim <- function(x, xlab = "", ...) {
  # Save old state
  old.par <- par(no.readonly=T)

  # Get quantities of interest
  qi <- x$qi

  # These qi's are considered special, since they can be plotted together
  ev.titles <- c('Expected Values: E(Y|X)', 'Expected Values (for X1): E(Y|X1)')
  pv.titles <- c('Predicted Values: Y|X', 'Predicted Values (for X1): Y|X1')

  if (is.null(x$x)) {
    return(par(old.par))
  }

  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1:2, nrow=2, ncol=2)
    palette <- c('gray', 'gray')

    # the plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }

  else {

    panels <- matrix(c(1:5, 5), ncol=2, nrow=3, byrow = TRUE)

    red <- rgb(242, 122, 94, maxColorValue=255)
    blue <- rgb(100, 149, 237, maxColorValue=255)

    palette <- c(red, blue, red, blue, 'gray', 'gray')

    # Determine whether two "Expected Values" qi's exist
    ev.bool <- all(ev.titles %in% names(qi))
    # Determine whether two "Predicted Values" qi's exist
    pv.bool <- all(pv.titles %in% names(qi))

    panels <- if (xor(ev.bool, pv.bool))
      rbind(panels, c(6, 6))

    else if (ev.bool && pv.bool)
      rbind(panels, c(6, 7))

    # the plotting device:
    #
    # +-----------+     +-----------+
    # |  1  |  2  |     |  1  |  2  |
    # +-----+-----+     +-----+-----+
    # |  3  |  4  |     |  3  |  4  |
    # +-----+-----+ OR  +-----+-----+
    # |     5     |     |     5     |
    # +-----------+     +-----------+
    # |  6  |  7  |     |     6     |
    # +-----+-----+     +-----+-----+
  }

  # Set layout
  layout(panels)


  # Get unsummarized qi data
  labels <- names(attr(qi, '.index'))
  k <- 0
  size <- length(labels)

  # Loop through qi's
  for (key in labels) {
    # Change names for code clarity
    val <- qi[[key]]

    if (all(is.na(val))) 
      next

    if (k >= length(palette))
      k <- 0

    color <- palette[k <- k + 1]

    if (all(is.na(val)))
      next

    else
      simulations.plot(val, main = key, col = color, line.col = "black")
  }

  if (all(ev.titles %in% names(qi))) {
    # Display 2 plots in one pane
    simulations.plot(
      qi[[ ev.titles[[1]] ]],
      qi[[ ev.titles[[2]] ]],
      main = "Comparison between E(Y|X) and E(Y|X1)",
      line.col = "black", col = c(red, blue)
      )
  }

  if (all(pv.titles %in% names(qi))) {
    # Display 2 plots in one pane
    simulations.plot(
      qi[[ pv.titles[[1]] ]],
      qi[[ pv.titles[[2]] ]],
      main = "Comparison between Y|X and Y|X1",
      line.col = "black", col = c(red, blue)
      )
  }

  # Restore old state
  par(old.par)
}
