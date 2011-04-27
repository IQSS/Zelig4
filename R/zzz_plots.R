#' Receiver Operator Characteristic Plots
#'
#' The 'rocplot' command generates a receiver operator characteristic plot to
#' compare the in-sample (default) or out-of-sample fit for two logit or probit
#' regressions.
#'
#' @usage
#' rocplot(
#'         y1, y2, fitted1, fitted2,
#'         cutoff=seq(from = 0, to = 1, length =100),
#'         lty1="solid", lty2="dashed", lwd1=par("lwd"),
#'         lwd2=par("lwd"), col1=par("col"), col2=par("col"),
#'         main="ROC Curve", xlab="Proportion of 1's Correctly Predicted",
#'         ylab="Proportion of 0's Correctly Predicted", plot=TRUE,
#'         ...)
#'
#' @param y1 response variable for the first model
#' @param y2 response variable for the second model
#' @param fitted1 fitted values for the first model. These values may represent
#'   either the in-sample or out-of-sample fitted values
#' @param fitted2 fitted values for the second model
#' @param cutoff A vector of cut-off values between 0 and 1, at which to
#'   evaluate the proportion of 0s and 1s correctly predicted by the first and
#'   second model.  By default, this is 100 increments between 0 and 1
#'   inclusive
#' @param lty1 the line type of the first model (defaults to 'line')
#' @param lty2 the line type of the second model (defaults to 'dashed')
#' @param lwd1 the line width of the first model (defaults to 1)
#' @param lwd2 the line width of the second model (defaults to 1)
#' @param col1 the color of the first model (defaults to 'black')
#' @param col2 the color of the second model (defaults to 'black')
#' @param main a title for the plot (defaults to "ROC Curve")
#' @param xlab a label for the X-axis
#' @param ylab a lavel for the Y-axis
#' @param plot whether to generate a plot to the selected device
#' @param \dots additional parameters to be passed to the plot
#' @return if plot is TRUE, rocplot simply generates a plot. Otherwise, a list
#'   with the following is produced:
#'   \item{roc1}{a matrix containing a vector of x-coordinates and
#'     y-coordinates corresponding to the number of ones and zeros correctly
#'     predicted for the first model.}
#'   \item{roc2}{a matrix containing a vector of x-coordinates and
#'     y-coordinates corresponding to the number of ones and zeros correctly
#'     predicted for the second model.}
#'   \item{area1}{the area under the first ROC curve, calculated using
#'     Reimann sums.}
#'   \item{area2}{the area under the second ROC curve, calculated using
#'     Reimann sums.}
#' @export
#" @author Kosuke Imai and Olivia Lau
rocplot <- function(y1, y2, fitted1, fitted2,
                    cutoff = seq(from=0, to=1, length=100), lty1="solid",
                    lty2="dashed", lwd1=par("lwd"), lwd2=par("lwd"),
                    col1=par("col"), col2=par("col"),
                    main="ROC Curve",
                    xlab = "Proportion of 1's Correctly Predicted",
                    ylab="Proportion of 0's Correctly Predicted", plot = TRUE, ...) {
  roc1 <- roc2 <- matrix(NA, nrow = length(cutoff), ncol = 2)
  colnames(roc1) <- colnames(roc2) <- c("ones", "zeros")
  for (i in 1:length(cutoff)) {
    roc1[i,1] <- mean(fitted1[y1==1] >= cutoff[i]) 
    roc2[i,1] <- mean(fitted2[y2==1] >= cutoff[i])
    roc1[i,2] <- mean(fitted1[y1==0] < cutoff[i])
    roc2[i,2] <- mean(fitted2[y2==0] < cutoff[i])
  }
  if (plot) {
    plot(0:1, 0:1, type = "n", xaxs = "i", yaxs = "i",
         main=main, xlab=xlab, ylab=ylab, ...)
    lines(roc1, lty = lty1, lwd = lwd1, col=col1)
    lines(roc2, lty = lty2, lwd = lwd2, col=col2)
    abline(1, -1, lty = "dotted")
  }
  else {
    area1 <- area2 <- array()
    for (i in 2:length(cutoff)) {
      area1[i-1] <- (roc1[i,2] - roc1[(i-1),2]) * roc1[i,1] 
      area2[i-1] <- (roc2[i,2] - roc2[(i-1),2]) * roc2[i,1] 
    }
    return(list(roc1 = roc1, 
                roc2 = roc2,
                area1 = sum(na.omit(area1)),
                area2 = sum(na.omit(area2))))
  }
}
