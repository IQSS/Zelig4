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
  #
  qi.list <- list()

  # Extract quantities of interest from the result set
  for (label in names(x)) {
    # Copy qi object for readability (does not perform a copy operation), since
    # no information is modified.
    q <- x[[label]]$qi

    # A list that pairs "long names", e.g. "Expected Values: E(Y|X)" with
    # "short names", e.g. "ev1".
    # This is to maintain backwards compatibility with Zelig 3.5
    .index <- attr(q, ".index")
    .list <- as.list(q)

    # Create a list to hold all the quantities of interest.
    # Each key represents a new "qi" which contains two slots:
    #  1. title, a human-readable character-string
    #  2. qi, a vector or matrix of simulations
    #  3. slot.name, a characer-string used to specify the short-form of the
    #     title (typically an acronymn + a number)
    # Note: This list is temporary, and used for readability. However, this should
    # not affect performance.
    qi.list <- list()

    # Iterate through each slot
    for (title in names(.index)) {
      slot.name <- .index[[title]]
      qi.list[[slot.name]] <- list(q = q[[title]], title = title, short = slot.name)
    }

    # pooled.qi.list[[label]] <- 
  }


  for (key in names(qi.list)) {
  }
  print(names(x$qi))
  print(attr(x$qi, '.index'))

  # Ensure that the confidence interval is in [0, 100]
  CI <- min(max(CI, 0), 100)

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
    # Get setx objects, which will eventually be converted into matrices
    m <- x[[key]]$x
    m1 <- x[[key]]$x1

    # If "x" exists, convert the setx object to a matrix and append it to 
    # the entire one. Note x1 is the setx object from the "sim" method.
    if (!is.null(m))
      x.matrix <- rbind(x.matrix, as.matrix(m))

    # If "x1" exists, convert the setx object to a matrix and append it to 
    # the entire one. Note x1 is the setx object from the "sim" method.
    if (!is.null(m1))
      x1.matrix <- rbind(x1.matrix, as.matrix(m1))
  }

  # Remove any rownames that might exist, and default to the [k, ] convention
  rownames(x.matrix) <- rownames(x1.matrix) <- NULL

  # Rough draft for tomorrow
  summarized.qi <- .summarize()

}

getQi <- function () {
}
