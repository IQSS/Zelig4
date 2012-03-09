plot.zelig.relogit <- function(x, xlab ="", user.par = FALSE, alt.col = "red",
                               ylab = NULL, samples = 100, ...){
  k <- length(x$qi)
  op <- par(no.readonly = TRUE)
  if (!user.par) 
    par(mar = c(4,4,2,1), tcl = -0.25, mgp = c(2, 0.6, 0))
  par(mfrow = c(k, 1))
  if (dim(x$qi[[1]])[2] == 1) {
    pr <- x$qi$pr
    y0 <- 100 * sum(pr == 0)/length(pr)
    y1 <- 100 * sum(pr == 1)/length(pr)
    barplot(c(y0, y1), horiz = TRUE, col = alt.col, las = 1,
            names.arg = c("Y = 0", "Y = 1"),
            xlab = "Percentage of Simulations",
            main = x$qi.name$pr, xlim = c(0, 100))
    x$qi$pr <- x$qi.name$pr <- NULL
    for (i in 1:(k-1)) {
      qi <- as.vector(x$qi[[i]])
      plot(density(qi), main = x$qi.name[[i]], xlab = xlab, ...)
    }    
  }
  else {
    for (i in 1:k) {
      qi <- x$qi[[i]]
      main <- as.character(x$qi.name[i])
      if (is.null(rownames(qi)))
        rownames(qi) <- 1:dim(qi)[1]
      idx <- as.integer(sample(rownames(qi), 100))
      tmp <- qi[idx,,1]
      xlim <- c(min(qi[,1,1]), max(qi[,2,1]))
      if (is.null(ylab))
        ylab <- paste("Observations (n = ", samples, ")", sep = "")
      plot(xlim, type = "n", xlab = xlab, ylab = ylab,
           main = main, ylim = c(0, 100), xlim = xlim, ...)
      for (j in 1:nrow(tmp))
        lines(c(tmp[j,1], tmp[j,2]), c(j,j), col = alt.col)
      abline(v = mean(qi[,1,1]))
      abline(v = mean(qi[,2,1]))
    }
  }
  par(op)
}
