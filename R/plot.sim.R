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

  # get iterator object
  i <- iter(sim.object$qi)

  # 
  par(mfrow=c(ceiling(length(sim.object$qi)/2), 2))

  # all the beautiful colors
  palette <- 312:325
  colors <- iter(c("seagreen2", "skyblue", "gray"), recycle=T)

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

    #
    if (is.numeric(val)) {
      val <- as.numeric(val)
      truehist(val, main = key, xlab = xlab, h=100, ..., col=nextElem(colors))
    }
    else if (is.character(val) || is.factor(val)) {
      barplot(table(val), xlab=xlab, main=key, col=palette)
    }

    else if (is.na(val))
      next

    else
      warning()
  }

  # restore old state
  par(old.par)
}
