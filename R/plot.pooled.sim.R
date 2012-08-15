#' Method for plotting pooled simulations by confidence intervals
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

  Y <- simulation.matrix(x, "Expected Values: E(Y|X)")
  X <- as.matrix(attr(x, "pooled.setx"))

  print(dim(Y))
  print(dim(X))

}
