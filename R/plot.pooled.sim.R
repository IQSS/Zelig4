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
#' @author James Honaker, adapted by Matt Owen \email{mowen@@iq.harvard.edu}
plot.pooled.sim <- function (x, CI = 95, col = NULL, qi = "Expected Values", xlab = "", ...) {

  qi <- find.match(qi, attr(x, "titles"))

  if (is.na(qi)) {
    warning(
      "Warning the specified quantity of interest does not exist\n  Legal values are: ",
      paste(attr(x, "titles"), sep=", ")
      )

    invisible(NULL)
  }
    
  # Specify the colors
  if (missing(col))
    col <- c(
      rgb(100, 149, 237, alpha=50, maxColorValue=255),
      rgb(152, 245, 255, alpha=50, maxColorValue=255),
      rgb(191, 239, 255, alpha=70, maxColorValue=255)
    )


  # Get the Upper Boundary of the Confidence Interval Defined by Alpha
  # @param x a vector of values
  # @param alpha a numeric, specifying the size of the confidence interval.
  # @return the value at the upper boundary
  ci.lower <- function(x, alpha)
    return(sort(x)[round((1-alpha)*length(x))])

  # Get the Lower Boundary of the Confidence Interval Defined by Alpha
  # @param x a vector of values
  # @param alpha a numeric, specifying the size of the confidence interval.
  # @return the value at the upper boundary
  ci.upper <- function(x, alpha)
    return(sort(x)[max(1,round(alpha*length(x)))])

  # Extract the forms as matrices
  Y <- simulation.matrix(x, qi)
  X <- as.matrix(attr(x, "pooled.setx"))

  # Specify the dimensions of the matrix containing the observed simulations
  history <- matrix(NA, nrow(X), 8)

  # Name the columns of our matrix
  colnames(history) <- c(
    "Value", "Median", "Upper 80%", "Lower 80%",
    "Upper 95%", "Lower 95%", "Upper 99.9%", "Lower 99.9%"
    )

  # Matrix of quantities of interest
  qi.matrix <- simulation.matrix(x, qi)

  # Get columns that don't vary
  qi.unique <- list()

  # Find out which column varies
  for (col in colnames(X))
    qi.unique[col] <- length( unique(X[, col]) ) > 1

  unique.column <- names(Filter(function (x) x == TRUE, qi.unique))

  if (length(unique.column) > 1) {
    warning("Too many columns vary")
    invisible(NA)
  }

  # Yes, this is equivalent to !length(unique.columns), but I like readability
  else if (length(unique.column) == 0) {
    warning("No columns vary")
    invisible(NA)
  }

  for (k in 1:nrow(X)) {

    # Specify values for code-clarity
    vals <- Y[, k]

    # Specify explanatory value
    history[k, "Value"] <- X[k, unique.column]

    # Specify median value
    history[k, "Median"] <- median(vals)

    # 80% CI
    history[k, "Upper 80%"] <- ci.upper(vals, .8)
    history[k, "Lower 80%"] <- ci.lower(vals, .8)

    # 95% CI
    history[k, "Upper 95%"] <- ci.upper(vals, .95)
    history[k, "Lower 95%"] <- ci.lower(vals, .95)

    # 99.9% CI
    history[k, "Upper 99.9%"] <- ci.upper(vals, .999)
    history[k, "Lower 99.9%"] <- ci.lower(vals, .999)
  }

  #
  history
}
