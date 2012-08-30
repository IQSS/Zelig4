#' Method for plotting pooled simulations by confidence intervals
#'
#' Plot pooled simulated quantities of interest.
#' @usage \method{plot}{pooled.sim}(x, xlab = "", ...)
#' @S3method plot pooled.sim
#' @param x A `sim' object
#' @param CI A number, between 0 and 100, specifying the confidence interal to
#' construct
#' @param xlab Labels for the x-axis
#' @param ... Parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @param leg.pos ``legend type'', exact coordinates and sizes for legend.
#' Overrides argment ``leg.type''
#' @param leg.type ``legend position'', an integer from 1 to 4, specifying the
#' position of the legend. 1 to 4 correspond to ``SE'', ``SW'', ``NW'', and
#' ``NE'' respectively
#' @param leg.col ``legend color'', an valid color used for plotting the line
#' colors in the legend
#' @return the current graphical parameters. This is subject to change in future
#' implementations of Zelig
#' @author James Honaker, adapted by Matt Owen \email{mowen@@iq.harvard.edu}
plot.pooled.sim <- function (x, CI = 95, col = NULL, qi = "Expected Values", xlab = "", ..., leg.pos = NULL, leg.type = 1, leg.col = "gray30") {

  # Specify graphical parameters
  par(bty = "n")

  # Get the closest fit to an exact match for a quantity of interest
  qi <- find.match(qi, attr(x, "titles"))

  # If there is no match at all, return an error
  if (is.na(qi)) {
    warning(
      "Warning the specified quantity of interest does not exist\n  Legal values are: ",
      paste(attr(x, "titles"), sep=", ")
      )

    invisible(NULL)
  }
    
  # Specify the colors, if none are given
  if (is.null(col))
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
  for (column in colnames(X))
    qi.unique[column] <- length( unique(X[, column]) ) > 1

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

  # Get limits for plot
  plot.domain <- c(min(history[, 1]), max(history[, 1]))
  plot.range <- c(min(history[, -1]), max(history[, -1]))
  plot.label <- paste("Range of \"", unique.column, "\"", sep="")

  #
  plot(
    # Plot points on an x-y axis. Check out this semantic programming
    history[, "Value"], history[, "Median"],
    # Specify domain and range of plot
    xlim = plot.domain, ylim = plot.range,
    # Label the axes
    xlab = plot.label, ylab = qi,
    # Title the plot
    main = "Confidence Intervals with Respect to Explanatory Variables"
    )


  # Values
  k <- ncol(Y)
  x.coords <- c(history[, 1], history[k:1, 1])

  # Draw region around 80% confidence interval (NOTE: 3 = Upper 80%, 4 = Lower 80%)
  polygon(x.coords, c(history[, 3], history[k:1, 4]), col=col[1], border="gray60")

  # Draw region around 95% confidence interval (NOTE: 5 = Upper 80%, 6 = Lower 80%)
  polygon(x.coords, c(history[, 5], history[k:1, 6]), col=col[2], border="gray90")

  # Draw region around 99.9% confidence interval (NOTE: 7 = Upper 80%, 8 = Lower 80%)
  polygon(x.coords, c(history[, 7], history[k:1, 8]), col=col[3], border="white")



    ## This is the legend

  if(is.null(leg.pos)) {
    leg.pos <- if (leg.type == 1)
      c(.91, .04, .2, .05)
    else if (leg.type == 2)
      c(.09, .04, .2, .05)
    else if (leg.type == 3)
      c(.09, .04, .8, .05)
    else
      c(.91, .04, .8, .05)
  }

  # Construct positions for legend
  lx <- plot.domain[1] + leg.pos[1] * (plot.domain[2] - plot.domain[1])
  hx <- plot.domain[1] + (leg.pos[1] + leg.pos[2]) * (plot.domain[2] - plot.domain[1])
  deltax <- (hx - lx)/10
  my <- plot.range[1] + leg.pos[3] * (plot.range[2] - plot.range[1])
  dy <- leg.pos[4] * (plot.range[2] - plot.range[1])

  # Draw the lines for the legend
  lines(c(hx+deltax,hx+2*deltax,hx+2*deltax,hx+deltax),c(my+3*dy,my+3*dy,my-3*dy,my-3*dy),col=leg.col)
  lines(c(hx+3*deltax,hx+4*deltax,hx+4*deltax,hx+3*deltax),c(my+1*dy,my+1*dy,my-1*dy,my-1*dy),col=leg.col)
  lines(c(lx-deltax,lx-2*deltax,lx-2*deltax,lx-deltax),c(my+2*dy,my+2*dy,my-2*dy,my-2*dy),col=leg.col)
  lines(c(lx-5*deltax,lx),c(my,my),col="white",lwd=3)
  lines(c(lx-5*deltax,lx),c(my,my),col=leg.col)
  lines(c(lx,hx),c(my,my))

  # Color in the lines drawn for the legend
  polygon(c(lx,lx,hx,hx),c(my-2*dy,my+2*dy,my+2*dy,my-2*dy),col=col[2],border="gray90")
  polygon(c(lx,lx,hx,hx),c(my-1*dy,my+1*dy,my+1*dy,my-1*dy),col=col[1],border="gray60")
  polygon(c(lx,lx,hx,hx),c(my-3*dy,my+3*dy,my+3*dy,my-3*dy),col=col[3],border="white")

  # Label the legend
  text(lx,my,labels="median",pos=2,cex=0.5,col=leg.col)
  text(lx,my+2*dy,labels="ci95",pos=2,cex=0.5,col=leg.col)
  text(hx,my+1*dy,labels="ci80",pos=4,cex=0.5,col=leg.col)
  text(hx,my+3*dy,labels="ci99.9",pos=4,cex=0.5,col=leg.col)

  # Fin.
}
