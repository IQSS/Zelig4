#' Method for plotting pooled simulations
#'
#' Plot pooled simulated quantities of interest.
#' @usage \method{plot}{pooled.sim}(x, xlab = "", ...)
#' @S3method plot pooled.sim
#' @param x a `sim' object
#' @param CI a number, between 0 and 100, specifying the confidence interal to
#' construct
#' @param xlab labels for the x-axis
#' @param ... parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @return the current graphical parameters. This is subject to change in future
#' implementations of Zelig
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.pooled.sim <- function (x, CI = 95, qi = "ev", xlab = "", ...) {

  # Get title/etc from "qi"
  if (qi %in% names(x$qi)) {
  }
  else if (qi %in% attr(x$qi, '.index')) {
  }
  else {
  }

  # Ensure that the confidence interval is in [0, 100]
  CI <- min(max(CI, 0), 100)

  # Create a range around the CI, and convert it to a value less than 1
  cip <- c(100-CI, 100+CI)/200

  # Generate confidence interval values
  # @param simulations a vector of matrix representing simulations
  # @param cip confidence interval... kinda
  # @return a matrix containing quantile information
  .summarize <- function (simulations, cip) {
    res <- apply(z, 2, quantile, prob=cip[1])
    res <- cbind(res, apply(z, 2, quantile, prob=cip[2]))
    res
  }

  x.matrix <- x1.matrix <- NULL

  # Construct a matrix of values
  for (key in names(x)) {
    m <- x[[key]]$x
    m1 <- x[[key]]$x1
    if (!is.null(m))
      x.matrix <- rbind(x.matrix, as.matrix(m))
    if (!is.null(m1))
      x1.matrix <- rbind(x1.matrix, as.matrix(m1))
  }

  rownames(x.matrix) <- rownames(x1.matrix) <- NULL

  # Rough draft for tomorrow
  summarized.qi <- .summarize()

  domain <- c(min(1), max(2))
  range <- c(min(summarized.qi), max(summarized.qi))


}
