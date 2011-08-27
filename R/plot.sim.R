#' Method for plotting simulations
#'
#' @S3method plot sim
#'
#' @param x a `sim' object
#' @param xlab labels for the x-axis
#' @param ... parameters to be passed to the `truehist' function
#'            which is implicitly called for numeric simulations
#' @return the current graphical parameters. This is subject to change in
#'         future implementations of Zelig
#' @export
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.sim <- function(x, xlab = "", ...) {
  # save old state
  old.par <- par(no.readonly=T)


  if (is.null(x$x)) {
    return(par(old.par))
  }

  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1:2, nrow=2, ncol=2)
    palette <- c('black', 'black')

    # the plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }

  else {

    panels <- matrix(c(1:5, 5), ncol=2, byrow = TRUE)
    palette <- c('red', 'navy', 'red', 'navy', 'black')

    # the plotting device:
    #
    # +-----------+
    # |  1  |  2  |
    # +-----+-----+
    # |  3  |  4  |
    # +-----+-----+
    # |     5     |
    # +-----------+
  }

  # Set layout
  layout(panels)


  # Get unsummarized qi data
  qi <- x$qi
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

    #
    if (all(is.na(val)))
      next

    else if (is.numeric(val)) {
      val <- as.numeric(val)
      plot(density(val), main = key, col=color)
    }

    else if (is.character(val) || is.factor(val)) {
      barplot(table(val), xlab=xlab, main=key, col=color)
    }

    else if (!is.na(val))
      warning('The Quantity of Interest "', key, '" has no valid printing method.')
  }

  # restore old state
  par(old.par)
}
