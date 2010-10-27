rocplot <- function(y1, y2, fitted1, fitted2,
                    cutoff = seq(from=0, to=1, length=100), lty1="solid",
                    lty2="dashed", lwd1=par("lwd"), lwd2=par("lwd"),
                    col1=par("col"), col2=par("col"), main="ROC Curve",
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
