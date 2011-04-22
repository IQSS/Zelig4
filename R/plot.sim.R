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
  sim.object <- x

  # save old state
  old.par <- par(no.readonly=T)


  if (is.null(x$x))
    return(par(old.par))

  else if (is.null(x$x1) || is.na(x$x1)) {

    panels <- matrix(1:2, nrow=2)
    palette <- c('black', 'black')

    # the plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }

  else {

    panels <- matrix(c(1, 3, 5, 2, 4, 5), ncol=2)
    palette <- c('red', 'navy', 'red', 'navy', 'black', 'black')

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

  layout(panels)


  # get iterator object
  i <- iter(sim.object$qi)

  # all the beautiful colors
  colors <- iter(palette, recycle=T)

  # loop through all the qi simulations
  repeat {
    # get key-value pair
    item <- try(nextElem(i), silent=T)

    # catch end of iterator
    if (inherits(item, "try-error"))
      break

    # for code-clarity
    key <- item$key
    val <- item$value
    color <- nextElem(colors)
    #
    if (is.numeric(val)) {
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
