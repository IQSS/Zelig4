##
## showzelig.r
##
## Monte Carlo data to test Zelig.
##
## jH 29/11/12
##

rm(list=ls())

library(Zelig)


#############################################
## General Parameters to set

nsim<- 1000           # Number of observations
minx<- -1             # Min x value
maxx<- 1              # Max x value

b1<-c(0.2,2,1)        # Set of speed parameters (coef on x) to cycle over
b0<-c(-1,0,2)         # Set of shift parameters (constants) to cycle over
trials<-c(3,6,6)      # Number of trials to cycle over, for distbns where relevant
alpha<-c(1,2,3)       # Auxiliary parameter to cycle over, where relevant
sd<-c(1,1,1)          # Standard Deviations to cycle over, where relevant

allnames<-cbind("ls","logit","probit","poisson","negbinom")  # Zelig models to cycle over

if("tobit" %in% allnames){
	library("survival")
}


#############################################
## Simulate Data

#par(mfcol=c(length(allnames),length(b1)))
par(mfrow=c(length(b1),length(allnames)))


for(i in 1:length(b1)){

  x.sim<- runif(nsim,minx,maxx)                       # These are simulated random x values
  x.seq<- seq(from=minx, to=maxx, length.out=nsim)    # This is a regular sequence across the range of x
  x.shortseq <- seq(from=minx,to=maxx, length.out=20) # This is for Zelig simulations
  
  data<-data.frame(cbind(x.sim,x.seq))                # Copy to dataset, but also leave as vectors
  data$x.junk<-rnorm(nsim)

  # Simulation of Linear Regression
  
  mu.sim<-b1[i]*x.sim + b0[i]
  data$y.sim.ls<-rnorm(nsim,mean=mu.sim,sd=sd[i])
  data$y.hat.ls<-b1[i]*x.seq + b0[i]

  # Simulation of Logit

  pi.sim<- 1/(1 + exp(-b1[i] * x.sim -b0[i])) 
  data$y.sim.logit<- rbinom(nsim,1,pi.sim)
  data$y.hat.logit<- 1/(1 + exp(-b1[i] * x.seq -b0[i]))

  # Simulation of Probit

  pi.sim<- pnorm( b1[i] * data$x.sim +b0[i] ) 
  data$y.sim.probit<- rbinom(nsim,1,pi.sim)
  data$y.hat.probit<- pnorm( b1[i] * x.seq +b0[i] ) 

  # Simulation of Binomial

  pi.sim<- 1/(1 + exp(-b1[i] * x.sim -b0[i])) 
  data$y.sim.binom<- rbinom(nsim,trials[i],pi.sim)
  data$y.hat.binom<- (1/(1 + exp(-b1[i] * x.seq -b0[i])) ) * trials[i]

  # Simulation of Poisson

  lambda.sim<- exp(b1[i] * x.sim +b0[i])
  data$y.sim.poisson<- rpois(nsim,lambda.sim)
  data$y.hat.poisson<- exp(b1[i] * x.seq +b0[i])

  # Simulation of Negative Binomial

  lambda.sim<- exp(b1[i] * x.sim +b0[i])
#  data$y.sim.negbinom<- rnbinom(n=nsim,mu=lambda.sim,size=alpha[i])
  data$y.sim.negbinom<- rnegbin(n=nsim,mu=lambda.sim,theta=alpha[i])
  data$y.hat.negbinom<- exp(b1[i] * x.seq +b0[i])
  

  # Simulation of Tobit

  mu.sim<- b1[i]*x.sim + b0[i]
  ystar.sim<-rnorm(nsim,mean=mu.sim,sd=sd[i])
  data$y.sim.tobit<- (ystar.sim>0) * ystar.sim
  mu.seq<- b1[i]*x.seq + b0[i] 
  y.uncensored.hat.tobit<- mu.seq + dnorm(mu.seq,mean=0,sd=sd[i])/pnorm(mu.seq,mean=0,sd=sd[i])
  data$y.hat.tobit<- y.uncensored.hat.tobit * (1- pnorm(0,mean=mu.seq,sd=sd[i]) ) 
  

#############################################
## Visualize Results

  for(j in 1:length(allnames)){

    ## Plot Monte Carlo Data

    y.sim.name<- paste("y.sim.",allnames[j],sep="")
    y.hat.name<- paste("y.hat.",allnames[j],sep="")
    all.main<-paste(allnames[j]," (",b1[i],",",b0[i],")",sep="")
    all.ylim<-c( min(c(data[,y.sim.name],data[,y.hat.name])) , max(c(data[,y.sim.name],data[,y.hat.name])) )

    plot(data$x.sim,data[,y.sim.name],main=all.main,ylim=all.ylim,xlab="x",ylab="y",col="steelblue")
    par(new=TRUE)
    plot(data$x.seq,data[,y.hat.name],main="",ylim=all.ylim,xlab="",ylab="",type="l",col="firebrick",lwd=2)

    ## Estimate and Plot Zelig Model
   
    zeligVarnames<- c("x.sim","x.junk",y.sim.name)
    
    subsetdata<- data[,zeligVarnames]

    formula<-as.formula(paste(y.sim.name," ~ x.sim + x.junk",sep=""))
#    formula<-as.formula(paste(y.sim.name," ~ x.sim",sep=""))

    z.out<-zelig(formula,model=allnames[j],data=subsetdata)

    ## Simulate using the "ci" approach. 
	z.set<-setx(z.out,x.sim=x.shortseq)
    z.sim<-sim(z.out,x=z.set)
    
    y.qiseq<-rep(-999,length(x.shortseq))
    for(s in 1:length(x.shortseq)){
        y.qiseq[s]<-mean(z.sim[[s]]$qi$ev1)    # Although, curious why ..$qi[1] is printable but mean = NA
    	
    }
    points(x=x.shortseq,y=y.qiseq,col="blue",cex=1.6)


    ## Iterate by the conventional "sim" across values.
    y.qiseq<-rep(-999,length(x.shortseq))
    y.qiseq2<-rep(-999,length(x.shortseq))

    for(s in 1:length(x.shortseq)){
		z.set<-setx(z.out,x.sim=x.shortseq[s])
    	z.sim<-sim(z.out,x=z.set)
        y.qiseq[s]<-mean(z.sim$qi$ev1)   
        
    }
    points(x=x.shortseq,y=y.qiseq,col="green",cex=1)
    dev.copy2pdf(file="./figs/mcz41.pdf")

  } 
}
