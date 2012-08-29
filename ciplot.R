##
##  ciplot
## 
##
##  Description
##
##   Draw a plot of bounds of confidence intervals of expected values across a range of values of some covariate of interest  
##
##  Usage
##
##    ciplot<-function(x,ev,col=NULL,leg=1,legcol="gray30",legpos=NULL)
##
##  Arguments
##    x: Vector of values of a covariate of interest
##    ev: A matrix of expected values, where each column corresponds to simulations drawn for one covariate value.  If x is of length k, and there are n simulations of each expected value, then ev should be n-by-k.
##    leg: Simple position choice for legend. 1,2,3,4 imply SE,SW,NW,NE corners of the graph. "n" means no legend.
##    legcol: Color choice for all labeling and annotation in the legend
##    legpos: Exact coordinates and sizes for legend.  overrides argment leg. 
##      A vector of length four such as (a,b,c,d) where 0<a<1 is the fraction along the x-axis the left edge of the legend should appear, 0<b<1 is the fraction of the x-axis the width of the legend should consume, 0<c<1 is the fraction of y-axis at which the center should appear, and 0<d<1 scales the height of the legend.
##      for example, leg=1 implies legpos=c(.91,.04,.2,.05)   
##
##  30/7/12, jH



ciplot<-function(x,ev,legcol="gray30",col=NULL,leg=1,legpos=NULL){

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
    history[i,]<-c(x[i,"age"],median(ev[,i]),ci.upper(ev[,i],0.8),ci.lower(ev[,i],0.8),ci.upper(ev[,i],0.95),ci.lower(ev[,i],0.95),ci.upper(ev[,i],0.999),ci.lower(ev[,i],0.999))
  }
  all.xlim<-c(min(history[,1]),max(history[,1]))
  all.ylim<-c(min(history[,-1]),max(history[,-1]))

  ## This is the plot

  par(bty="n")

  plot(x=history[,1],y=history[,2],type="l",xlim=all.xlim,ylim=all.ylim,xlab="Range of Age",ylab="Expected Values: E(Y|X)")

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




######################################
## Example

library(Zelig)	 # This example will only work in Zelig3					

set.seed(873)
data<-read.table("votedata.dat")
data<-as.data.frame(data)

z.out <- zelig(gen94 ~ nsex + age, model = "logit", data = data) 

# Example of full range

par(mfrow=c(1,1))
age.range<-18:100
xc.range<- setx(z.out, age=age.range)                
s.out <- sim(z.out, x = xc.range)
plot.ci(s.out)

par(ask=TRUE)
par(mfrow=c(1,2))

x<-s.out$x
ev<-s.out$qi$ev
ciplot(x=x,ev=ev)

dev.copy2pdf(file="plotcizoom4.pdf")

# Example over range subset

age.range<-80:95
xc.range<- setx(z.out, age=age.range)                  
s.out <- sim(z.out, x = xc.range)

x<-s.out$x
ev<-s.out$qi$ev
ciplot(x=x,ev=ev,leg=3)

dev.copy2pdf(file="ciplot.pdf")



