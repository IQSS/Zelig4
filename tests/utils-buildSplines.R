library(Zelig)

n<-100
x<-runif(n)
time<-seq(from=1,to=10,length=n)
y<--3.5*time + 0.4*time^2 + x + rnorm(n)
mydata<-as.data.frame(cbind(y,x,time))

myformula<-"y ~ x + s(time)"

output<-zeligBuildSpline(formula=myformula, k=5, data=mydata)
