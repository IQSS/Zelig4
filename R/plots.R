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
  .plot.density(ev1, "Expected Values: E(Y|X)", palette[1])

  if (!is.null(x$x1)) {
    .plot.density(ev2, "Expected Values: E(Y|X1)", palette[2])
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
#' Plot graphs of simulated multiply-imputed data
#'
#' This function combines results across multiply imputed results
#'    and then calls the appropriate plot for that class.
#'
#' @usage \method{plot}{MI.sim}(...)
#' @S3method plot MI.sim
#' @param ... ignored parameters
#'
#' @return the return of the appropriate plot method
#' @author James Honaker \email{jhonaker@@iq.harvard.edu}
plot.MI.sim <- function(x, ...) {

  m<-length(x)                           # The number of imputed datasets
  reformed<-x[[1]]                       # Simplified object of the original class
  all.qi<-attributes(x[[1]]$qi)$names    # Convoluted given the current structure of objects

  ## Currently, we're appending all the qi's together into one object
  ## Note - everything that is not a qi, will just come from the first imputed dataset
  
  if(m>1){
    for(i in 2:m){
      for(j in all.qi){    # Could do this by position number as "in 1:length(all.qi)"
        ## The $qi's are themselves lists, so this is difficult:
        reformed$qi[j][[1]]<-rbind(reformed$qi[j][[1]],x[[i]]$qi[j][[1]])
      }
    }
  }

  output<-plot(reformed)
  ## Return any plot returns invisibly
  invisible(output)
}


#' Method for plotting pooled simulations by confidence intervals
#'
#' Plot confidence intervals of pooled simulated values.
#' 
#' @param x A `sim' object
#' @param qi a character-string specifying the quantity of interest to plot
#' @param var The variable to be used on the x-axis. Default is the variable
#' across all the chosen values with smallest nonzero variance
#' @param ... Parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @param main a character-string specifying the main heading of the plot
#' @param sub a character-string specifying the sub heading of the plot
#' @param xlab a character-string specifying the label for the x-axis
#' @param ylab a character-string specifying the label for the y-axis
#' @param legcol ``legend color'', an valid color used for plotting the line
#' colors in the legend
#' @param col a valid vector of colors of at least length 3 to use to color the
#' confidence intervals
#' @param leg ``legend position'', an integer from 1 to 4, specifying the
#' position of the legend. 1 to 4 correspond to ``SE'', ``SW'', ``NW'', and
#' ``NE'' respectively
#' @param legpos ``legend type'', exact coordinates and sizes for legend.
#' Overrides argment ``leg.type''
#' @return the current graphical parameters. This is subject to change in future
#' implementations of Zelig
#' @author James Honaker, adapted by Matt Owen \email{mowen@@iq.harvard.edu}
#' @export plot.ci
#' @usage \method{plot}{ci}(x, qi="ev", var=NULL, ..., legcol="gray20", col=NULL, leg=1, legpos=NULL)
plot.ci <- function(x, qi="ev", var=NULL, ..., main = NULL, sub = NULL, xlab = NULL, ylab = NULL, xlim = NULL, ylim = NULL, legcol="gray20", col=NULL, leg=1, legpos=NULL, ci=c(80,95,99.9)) {

  if(length(ci)<3){
  	ci<-rep(ci,3)
  }
  if(length(ci)>3){
  	ci<-ci[1:3]
  }
  ci<-sort(ci)

  if (! "pooled.sim" %in% class(x)) {
    something <- list(x=x)
    class(something) <- "pooled.sim"
    attr(something, "titles") <- x$titles
    x <- something
  }

  xmatrix<-matrix(NA,nrow=length(x),ncol=length(x[[1]]$x$data))

  for(i in 1:length(x)){
    xmatrix[i,]<-as.matrix(x[[i]]$x$data)
  }

  if (length(x) == 1 && is.null(var)) {
    warning("Must specify the `var` parameter when plotting the confidence interval of an unvarying model. Plotting nothing.")
    return(invisible(FALSE))
  }

  if(is.character(var)){
    if( !(var %in% names(x[[1]]$x$data) ) ){
      warning("Specified variable for confidence interval plot is not in estimated model.  Plotting nothing.")
      return(invisible(FALSE))
    }  
  }


  if (is.null(var)) {
    each.var <- apply(xmatrix,2,sd) 
    flag <- each.var>0
    min.var<-min(each.var[flag])
    var.seq<-1:ncol(xmatrix)
    position<-var.seq[each.var==min.var]
  } else {
    if(is.numeric(var)){   
      position<-var
    }else if(is.character(var)){
      position<-grep(var,names(x[[1]]$x$data))
    }
  }
  position<-min(position)
  xseq<-xmatrix[,position]
  xname<-names(x[[1]]$x$data[position])


  # Use "qi" argument to select quantities of interest and set labels
  ev1<-NULL
  if(qi=="pv"){
    request<-"Predicted Values: Y|X"
    if(!is.null(x[[1]]$x1)){
      ev1<-simulation.matrix(x, "Predicted Values: Y|X1")
    }
  } else if(qi=="fd") {
    request<-"First Differences: E(Y|X1) - E(Y|X)"
  } else {
    request<-"Expected Values: E(Y|X)"
    if(!is.null(x[[1]]$x1)){
      ev1<-simulation.matrix(x, "Expected Values: E(Y|X1)")
    }
  }
  ev<-simulation.matrix(x, request)
  if (is.null(ylab)){
    ylab <- request
  }


  # Define functions to compute confidence intervals
  ci.upper <- function (x, alpha) {
    pos <- max(round((1-(alpha/100))*length(x)), 1)
    return(sort(x)[pos])
  }

  ci.lower <- function (x, alpha) {
    pos<-max(round((alpha/100)*length(x)), 1)
    return(sort(x)[pos])
  }

  #
  k<-ncol(ev)
  n<-nrow(ev)

  #
  if(is.null(col)){
    myblue1<-rgb( 100, 149, 237, alpha=50, maxColorValue=255)
    myblue2<-rgb( 152, 245, 255, alpha=50, maxColorValue=255)
    myblue3<-rgb( 191, 239, 255, alpha=70, maxColorValue=255)
    myred1 <-rgb( 237, 149, 100, alpha=50, maxColorValue=255)
    myred2 <-rgb( 255, 245, 152, alpha=50, maxColorValue=255)
    myred3 <-rgb( 255, 239, 191, alpha=70, maxColorValue=255)

    col<-c(myblue1,myblue2,myblue3,myred1,myred2,myred3)
  }else{
  	if(length(col)<6){
  	  col<-rep(col,6)[1:6]
    }
  }


  form.history <- function (k,xseq,results,ci=c(80,95,99.9)){
  
    history<-matrix(NA, nrow=k,ncol=8)
    for (i in 1:k) {
      v <- c(
             xseq[i],
             median(results[,i]),
 
             ci.upper(results[,i],ci[1]),
             ci.lower(results[,i],ci[1]),

             ci.upper(results[,i],ci[2]),
             ci.lower(results[,i],ci[2]),

             ci.upper(results[,i],ci[3]),
             ci.lower(results[,i],ci[3])
             )

      history[i, ] <- v
    }
    if (k == 1) {
      left <- c(
             xseq[1]-.5,
             median(results[,1]),

             ci.upper(results[,1],ci[1]),
             ci.lower(results[,1],ci[1]),

             ci.upper(results[,1],ci[2]),
             ci.lower(results[,1],ci[2]),

             ci.upper(results[,1],ci[3]),
             ci.lower(results[,1],ci[3])
             )
      right <- c(
             xseq[1]+.5,
             median(results[,1]),

             ci.upper(results[,1],ci[1]),
             ci.lower(results[,1],ci[1]),

             ci.upper(results[,1],ci[2]),
             ci.lower(results[,1],ci[2]),

             ci.upper(results[,1],ci[3]),
             ci.lower(results[,1],ci[3])
             )
      v <- c(
             xseq[1],
             median(results[,1]),

             ci.upper(results[,1],ci[1]),
             ci.lower(results[,1],ci[1]),

             ci.upper(results[,1],ci[2]),
             ci.lower(results[,1],ci[2]),

             ci.upper(results[,1],ci[3]),
             ci.lower(results[,1],ci[3])
             )
      history <- rbind(left, v, right)
    }

    return(history)
  }

  history<-  form.history(k,xseq,ev,ci)
  if(!is.null(ev1)){
    history1<- form.history(k,xseq,ev1,ci)
  }else{
    history1<-NULL
  }

  # This is for small sets that have been duplicated so as to have observable volume
  if(k==1){
    k<-3
  }

  # Specify x-axis length
  all.xlim <- if (is.null(xlim))
    c(min(c(history[, 1],history1[, 1])),max(c(history[, 1],history1[, 1])))
  else
    xlim


  # Specify y-axis length
  all.ylim <-if (is.null(ylim))
    c(min(c(history[, -1],history1[, -1])),max(c(history[, -1],history1[, -1])))
  else
    ylim


  # Define xlabel
  if (is.null(xlab))
    xlab <- paste("Range of",xname)

  if (is.null(ylab))
    ylab <- "Expected Values: E(Y|X)"

  ## This is the plot

  par(bty="n")

  plot(x=history[, 1], y=history[, 2], type="l", xlim=all.xlim, ylim=all.ylim, main = main, sub = sub, xlab=xlab, ylab=ylab)

  polygon(c(history[,1],history[k:1,1]),c(history[,7],history[k:1,8]),col=col[3],border="white")
  polygon(c(history[,1],history[k:1,1]),c(history[,5],history[k:1,6]),col=col[2],border="gray90")
  polygon(c(history[,1],history[k:1,1]),c(history[,3],history[k:1,4]),col=col[1],border="gray60")
  polygon(c(history[,1],history[k:1,1]),c(history[,7],history[k:1,8]),col=NA,border="white")

  if(!is.null(ev1)){
  lines(x=history1[, 1], y=history1[, 2], type="l")

  polygon(c(history1[,1],history1[k:1,1]),c(history1[,7],history1[k:1,8]),col=col[6],border="white")
  polygon(c(history1[,1],history1[k:1,1]),c(history1[,5],history1[k:1,6]),col=col[5],border="gray90")
  polygon(c(history1[,1],history1[k:1,1]),c(history1[,3],history1[k:1,4]),col=col[4],border="gray60")
  polygon(c(history1[,1],history1[k:1,1]),c(history1[,7],history1[k:1,8]),col=NA,border="white")

  }


  ## This is the legend

  if(is.null(legpos)){
    if(leg==1){
      legpos<-c(.91,.04,.2,.05)
    }else if(leg==2){
      legpos<-c(.09,.04,.2,.05)
    }else if(leg==3){
      legpos<-c(.09,.04,.8,.05)
    }else{
      legpos<-c(.91,.04,.8,.05)
    }
  }

  lx<-min(all.xlim)+ legpos[1]*(max(all.xlim)- min(all.xlim))
  hx<-min(all.xlim)+ (legpos[1]+legpos[2])*(max(all.xlim)- min(all.xlim))

  deltax<-(hx-lx)*.1

  my<-min(all.ylim) +legpos[3]*min(max(all.ylim) - min(all.ylim))
  dy<-legpos[4]*(max(all.ylim) - min(all.ylim))


  lines(c(hx+deltax,hx+2*deltax,hx+2*deltax,hx+deltax),c(my+3*dy,my+3*dy,my-3*dy,my-3*dy),col=legcol)
  lines(c(hx+3*deltax,hx+4*deltax,hx+4*deltax,hx+3*deltax),c(my+1*dy,my+1*dy,my-1*dy,my-1*dy),col=legcol)
  lines(c(lx-deltax,lx-2*deltax,lx-2*deltax,lx-deltax),c(my+2*dy,my+2*dy,my-2*dy,my-2*dy),col=legcol)
  lines(c(lx-5*deltax,lx),c(my,my),col="white",lwd=3)
  lines(c(lx-5*deltax,lx),c(my,my),col=legcol)
  lines(c(lx,hx),c(my,my))

  polygon(c(lx,lx,hx,hx),c(my-3*dy,my+3*dy,my+3*dy,my-3*dy),col=col[3],border="white")
  polygon(c(lx,lx,hx,hx),c(my-2*dy,my+2*dy,my+2*dy,my-2*dy),col=col[2],border="gray90")
  polygon(c(lx,lx,hx,hx),c(my-1*dy,my+1*dy,my+1*dy,my-1*dy),col=col[1],border="gray60")
  polygon(c(lx,lx,hx,hx),c(my-3*dy,my+3*dy,my+3*dy,my-3*dy),col=NA,border="white")

  text(lx,my,labels="median",pos=2,cex=0.5,col=legcol)
  text(lx,my+2*dy,labels=paste("ci",ci[2],sep=""),pos=2,cex=0.5,col=legcol)
  text(hx,my+1*dy,labels=paste("ci",ci[1],sep=""),pos=4,cex=0.5,col=legcol)
  text(hx,my+3*dy,labels=paste("ci",ci[3],sep=""),pos=4,cex=0.5,col=legcol)
}

#' Method for plotting pooled simulations by confidence intervals
#'
#' Plot pooled simulated quantities of interest.
#' @usage \method{plot}{pooled.sim}(x, qi="ev", var=NULL,  ...,  legcol="gray20", col=NULL, leg=1, legpos=NULL)
#' @S3method plot pooled.sim
#' @param x A `sim' object
#' @param qi a character-string specifying the quantity of interest to plot
#' @param var The variable to be used on the x-axis. Default is the variable
#' across all the chosen values with smallest nonzero variance
#' @param ... Parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @param legcol ``legend color'', an valid color used for plotting the line
#' colors in the legend
#' @param col a valid vector of colors of at least length 3 to use to color the
#' confidence intervals
#' @param leg ``legend position'', an integer from 1 to 4, specifying the
#' position of the legend. 1 to 4 correspond to ``SE'', ``SW'', ``NW'', and
#' ``NE'' respectively
#' @param legpos ``legend type'', exact coordinates and sizes for legend.
#' Overrides argment ``leg.type''
#' @return the current graphical parameters. This is subject to change in future
#' implementations of Zelig
#' @author James Honaker, adapted by Matt Owen \email{mowen@@iq.harvard.edu}
plot.pooled.sim <- plot.ci
#' Method for plotting simulations
#'
#' Plot simulated quantities of interest.
#' @usage \method{plot}{sim}(x, ...)
#' @S3method plot sim
#' @param x a `sim' object
#' @param ... parameters to be passed to the `truehist' function which is 
#' implicitly called for numeric simulations
#' @return nothing
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.sim <- function (x, ...) {

  env <- tryCatch(
    asNamespace(x$package.name),
    # 
    error = function (e) { 
      warning("")
      globalenv()
    }
    )

  # If plotPackageName
  if (exists("plot.simulations", envir = env, mode="function")) {
    # Get the simulation, because we know it exists
    .plotter <- get("plot.simulations", envir = env, mode="function")

    # Pass to a temporary variable to improve the visibility of the traceback
    # if there is an error
    res <- .plotter(x, ...)

    # Return object (whatever it is)
    return(invisible(res))
  }

  # Otherwise we just use this fall-back
  old.par <- par(no.readonly = T)

  # Some numbers we use to make things
  count<- 0
  for(i in 1:length(names(x$qi))){
      if (!all( is.na(x$qi[[i]]) ) & !is.null(x$qi[[i]]) )
      count<-count+1
      
  }
  total.qis<-max(count,1)
  #total.qis <- length(names(x$qi))

    
  palette <- rainbow(total.qis)
  total.cols <- 2
  total.rows <- ceiling(total.qis/total.cols)
  if(total.rows==1){
    total.rows<-2
    total.cols<-1
  }

  vals <- ifelse(total.qis %% 2, c(1:total.qis, total.qis), 1:total.qis)

  # Colors!
  color.blue <- rgb(100, 149, 237, maxColorValue=255)

  #
  vals <- if (total.qis %% 2) {
    c(1:total.qis, total.qis)
  }
  else {
    1:total.qis
  }

  # Construct layout
  layout(matrix(vals, total.rows, total.cols, byrow=TRUE))

  k <- 1
  for (title in names(x$qi)) {
    simulations.plot(x$qi[[title]], main = title, col = palette[k], line.col = "black")
    k <- k + 1
  }


  #
  return(par(old.par))
}

#' @S3method plot sim.cloglog.net
plot.sim.cloglog.net <- function (x, ...) {

  env <- tryCatch(
    asNamespace(x$package.name),
    error = function (e) { 
      warning("")
      globalenv()
    }
  )

  # If plotPackageName
  if (exists("plot.simulations", envir = env, mode="function")) {
    # Get the simulation, because we know it exists
    .plotter <- get("plot.simulations", envir = env, mode="function")

    # Pass to a temporary variable to improve the visibility of the traceback
    # if there is an error
    res <- .plotter(x, ...)

    # Return object (whatever it is)
    return(invisible(res))
  }

  # Otherwise we just use this fall-back
  old.par <- par(no.readonly = T)

  # Some numbers we use to make things
  total.qis <- length(names(x$qi))
  palette <- rainbow(total.qis)
  total.cols <- 2
  total.rows <- ceiling(total.qis/total.cols)

  vals <- ifelse(total.qis %% 2, c(1:total.qis, total.qis), 1:total.qis)

  # Colors!
  color.blue <- rgb(100, 149, 237, maxColorValue=255)

  #
  vals <- if (total.qis %% 2) {
    c(1:total.qis, total.qis)
  }
  else {
    1:total.qis
  }

  # Construct layout
  layout(matrix(vals, total.rows, total.cols, byrow=TRUE))

  k <- 1
  for (title in names(x$qi)) {
    simulations.plot(x$qi[[title]], main = title, col = palette[k], line.col = "black")
    k <- k + 1
  }


  #
  return(par(old.par))
}


#' Plot Any Simulation from the Zelig Core Package
#'
#' Plots any simulation from the core package. In general, this function can
#' \emph{neatly} plot simulations containing five of the popular ``quantities
#' of interest'' - ``Expected Values: E(Y|X)'', ``Predicted Values: Y|X'',
#' ``Expected Values (for X1): E(Y|X1)'', ``Predicted Values (for X1): Y|X1''
#' and ``First Differences: E(Y|X1) - E(Y|X)''.
#' @param x an object
#' @param ... parameters passed to the ``plot'' and ``barplot'' functions
#' @return the original graphical parameters
#' @author Matt Owen \email{mowen@@iq.harvard.edu}
plot.simulations <- function (x, ...) {
  # Save old state
  old.par <- par(no.readonly=T)
    
  # Quantities of Interest
  qi <- x$qi

  # Define Relevant quantity of interest titles that have special properties
  ev.titles <- c('Expected Values: E(Y|X)', 'Expected Values: E(Y|X1)')
  pv.titles <- c('Predicted Values: Y|X', 'Predicted Values: Y|X1')

  # Determine whether two "Expected Values" qi's exist
  both.ev.exist <- all(ev.titles %in% names(qi))
  # Determine whether two "Predicted Values" qi's exist
  both.pv.exist <- all(pv.titles %in% names(qi))

  # Color of x should always be this pertty blue
  color.x <- rgb(242, 122, 94, maxColorValue=255)
  color.x1 <- rgb(100, 149, 237, maxColorValue=255)

  # This mixes the above two colors, and converts the result into hexadecimal
  color.mixed <- rgb(t(round((col2rgb(color.x) + col2rgb(color.x1))/2)), maxColorValue=255)

  if (is.null(x$x)) {
    return(par(old.par))
  }
  else if (is.null(x$x1) || is.na(x$x1)) {
    panels <- matrix(1:2, 2, 1)

    # The plotting device:
    # +--------+
    # |   1    |
    # +--------+
    # |   2    |
    # +--------+
  }
  else {

    panels <- matrix(c(1:5, 5), ncol=2, nrow=3, byrow = TRUE)

    panels <- if (xor(both.ev.exist, both.pv.exist))
      rbind(panels, c(6, 6))
    else if (both.ev.exist && both.pv.exist)
      rbind(panels, c(6, 7))
    else
      panels


    # the plotting device:
    #
    # +-----------+    +-----------+
    # |  1  |  2  |    |  1  |  2  |
    # +-----+-----+    +-----+-----+
    # |  3  |  4  |    |  3  |  4  |
    # +-----+-----+ OR +-----+-----+
    # |     5     |    |     5     |
    # +-----------+    +-----------+
    # |  6  |  7  |    |     6     |
    # +-----+-----+    +-----+-----+
  }

  #
  layout(panels)

  titles <- list(
    ev  = "Expected Values: E(Y|X)",
    ev1 = "Expected Values: E(Y|X1)",
    pv  = "Predicted Values: Y|X",
    pv1 = "Predicted Values: Y|X1",
    fd  = "First Differences: E(Y|X1) - E(Y|X)"
    )
  
  # Plot each simulation
  simulations.plot(qi[[titles$pv]], main = titles$pv, col = color.x, line.col = "black")
  simulations.plot(qi[[titles$pv1]], main = titles$pv1, col = color.x1, line.col = "black")
  simulations.plot(qi[[titles$ev]], main = titles$ev, col = color.x, line.col = "black")
  simulations.plot(qi[[titles$ev1]], main = titles$ev1, col = color.x1, line.col = "black")
  simulations.plot(qi[[titles$fd]], main = titles$fd, col = color.mixed, line.col = "black")

  if (both.pv.exist) {
    simulations.plot(
      qi[["Predicted Values: Y|X"]],
      qi[["Predicted Values: Y|X1"]],
      main = "Comparison of Y|X and Y|X1",
      # Note that we are adding transparency to this
      col = paste(c(color.x, color.x1), "80", sep=""),
      line.col = "black")
  }

  if (both.ev.exist) {
    simulations.plot(
      qi[["Expected Values: E(Y|X)"]],
      qi[["Expected Values: E(Y|X1)"]],
      main = "Comparison of E(Y|X) and E(Y|X1)",
      # Note that we are adding transparency to this
      col = paste(c(color.x, color.x1), "80", sep=""),
      line.col = "black")
  }

  # Restore old state
  par(old.par)

  # Return old parameter invisibly
  invisible(old.par)
}


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
