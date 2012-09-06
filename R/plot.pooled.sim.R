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
plot.ci<-function(x,qi="ev",var=NULL, ..., legcol="gray20",col=NULL,leg=1,legpos=NULL){

  xmatrix<-matrix(NA,nrow=length(x),ncol=length(x[[1]]$x$data))
  for(i in 1:length(x)){
    xmatrix[i,]<-as.matrix(x[[i]]$x$data)
  }

  if(is.null(var)){
    each.var<-apply(xmatrix,2,sd) 
    flag<-each.var>0
    min.var<-min(each.var[flag])
    var.seq<-1:ncol(xmatrix)
    position<-var.seq[each.var==min.var]  
    position<-min(position)
    xseq<-xmatrix[,position]
    xname<-names(x[[1]]$x$data[position])
  }else{

    if(is.numeric(var)){
      position<-var
    }else if(is.character(var)){
      position<-grep(var,names(x[[1]]$x$data))
    }
    xseq<-xmatrix[,position]
    xname<-names(x[[1]]$x$data[position])
  }


  if(qi=="pv"){
    ev<-simulation.matrix(x, "Predicted Values: Y|X")
  }else{
    ev<-simulation.matrix(x, "Expected Values: E(Y|X)")
  }


  ## Set up defaults

  ci.upper<-function(x,alpha){
    pos<-round((1-alpha)*length(x))
    return(sort(x)[pos])
  }
  ci.lower<-function(x,alpha){
    pos<-max(1,round(alpha*length(x)))
    return(sort(x)[pos])
  }


  k<-ncol(ev)
  n<-nrow(ev)
  if(is.null(col)){
    myblue1<-rgb( 100, 149, 237, alpha=50, maxColorValue=255)
    myblue2<-rgb( 152, 245, 255, alpha=50, maxColorValue=255)
    myblue3<-rgb( 191, 239, 255, alpha=70, maxColorValue=255)
    col<-c(myblue1,myblue2,myblue3)
  }
  history<-matrix(NA, nrow=k,ncol=8)
  for(i in 1:k){
    history[i,]<-c(xseq[i],median(ev[,i]),ci.upper(ev[,i],0.8),ci.lower(ev[,i],0.8),ci.upper(ev[,i],0.95),ci.lower(ev[,i],0.95),ci.upper(ev[,i],0.999),ci.lower(ev[,i],0.999))
  }
  all.xlim<-c(min(history[,1]),max(history[,1]))
  all.ylim<-c(min(history[,-1]),max(history[,-1]))

  ## This is the plot

  par(bty="n")

  plot(x=history[,1],y=history[,2],type="l",xlim=all.xlim,ylim=all.ylim,xlab=paste("Range of",xname),ylab="Expected Values: E(Y|X)")

  polygon(c(history[,1],history[k:1,1]),c(history[,5],history[k:1,6]),col=col[2],border="gray90")
  polygon(c(history[,1],history[k:1,1]),c(history[,3],history[k:1,4]),col=col[1],border="gray60")
  polygon(c(history[,1],history[k:1,1]),c(history[,7],history[k:1,8]),col=col[3],border="white")

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

  polygon(c(lx,lx,hx,hx),c(my-2*dy,my+2*dy,my+2*dy,my-2*dy),col=col[2],border="gray90")
  polygon(c(lx,lx,hx,hx),c(my-1*dy,my+1*dy,my+1*dy,my-1*dy),col=col[1],border="gray60")
  polygon(c(lx,lx,hx,hx),c(my-3*dy,my+3*dy,my+3*dy,my-3*dy),col=col[3],border="white")

  text(lx,my,labels="median",pos=2,cex=0.5,col=legcol)
  text(lx,my+2*dy,labels="ci95",pos=2,cex=0.5,col=legcol)
  text(hx,my+1*dy,labels="ci80",pos=4,cex=0.5,col=legcol)
  text(hx,my+3*dy,labels="ci99.9",pos=4,cex=0.5,col=legcol)
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
