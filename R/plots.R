#' @S3method plot sim.gamma.gee
plot.sim.gamma.gee <- function (x, ...) {

  # store device settings
  original.par <- par(no.readonly=TRUE)

  if (is.null(x$x))
    return()

  panels <- if (is.null(x$x1)) {
    palette <- rep("black", 3)
    matrix(1, nrow=1, ncol=1)
    # How the layout window will look:
    # +---+
    # | 1 |
    # +---+
  }

  else {
    palette <- c('red', 'navy', 'black')
    matrix(c(1, 2, 3, 3), nrow=2, ncol=2, byrow=TRUE)
    # How the layout window will look:
    # +-------+
    # | 1 | 2 |
    # +-------+
    # |   3   |
    # +-------+
  }

  layout(panels)

  # extract quantities of interest
  ev1 <- x$qi$ev1
  ev2 <- x$qi$ev2
  fd <- x$qi$fd

  # Plot ev1
  .plot.density(ev1, "Expected Values (for X): E(Y|X)", palette[1])

  if (!is.null(x$x1)) {
    .plot.density(ev2, "Expected Values (for X1): E(Y|X1)", palette[2])
    .plot.density(fd, "First Differences: E(Y|X1) - E(Y|X)", palette[3])
  }
    
  # return plotting device
  par(original.par)
}

#' @S3method plot sim.normal.gee
plot.sim.normal.gee <- plot.sim.gamma.gee

#' @S3method plot sim.poisson.gee
plot.sim.poisson.gee <- plot.sim.gamma.gee

#' @S3method plot sim.logit.gee
plot.sim.logit.gee <- function (x, ...) {

  # store device settings
  original.par <- par(no.readonly=TRUE)

  if (is.null(x$x))
    return()

  panels <- if (is.null(x$x1)) {
    palette <- rep("black", 4)
    matrix(1, nrow=1, ncol=1)
    # How the layout window will look:
    # +---+
    # | 1 |
    # +---+
  }

  else {
    palette <- c('red', 'navy', 'black', 'black')
    matrix(c(1, 2, 3, 3, 4, 4), nrow=3, ncol=2, byrow=TRUE)
    # How the layout window will look:
    # +-------+
    # | 1 | 2 |
    # +-------+
    # |   3   |
    # +-------+
    # |   4   |
    # +-------+
  }

  layout(panels)

  # extract quantities of interest
  ev1 <- x$qi$ev1
  ev2 <- x$qi$ev2
  fd <- x$qi$fd
  rr <- x$qi$rr

  # Plot ev1
  .plot.density(ev1, "Expected Values (for X): E(Y|X)", palette[1])
  .plot.density(ev2, "Expected Values (for X1): E(Y|X1)", palette[2])
  .plot.density(fd, "First Differences: E(Y|X1) - E(Y|X)", palette[3])
  .plot.density(rr, "Risk Ratios: E(Y|X1)/E(Y|X)", palette[4])
    
  # return plotting device
  par(original.par)
}

#' @S3method plot sim.probit.gee
plot.sim.probit.gee <- plot.sim.logit.gee

# Plot Density Graphs for GEE Quantities of Interest
# @param x a vector containing quantities of interest
# @param main the main title of the plot
# @param col the color of the line-plot
.plot.density <- function (x, main, col) {
  if (all(is.na(x)))
    return()

  density <- density(x)
  plot(density(x), main = main, col = col)
}
