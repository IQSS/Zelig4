#####  Example 1: Basic Example with First Differences  #####

# Create some sample data:  
set.seed(0) 
n<-400
sig<-2
x0 <- runif(n, 0, 1)
x1 <- runif(n, 0, 1)
x2 <- runif(n, 0, 1)
x3 <- runif(n, 0, 1)
f0 <- function(x) 2 * sin(pi * x)
f1 <- function(x) exp(2 * x)
f2 <- function(x) 0.2*x^11*(10*(1-x))^6+10*(10*x)^3*(1-x)^10
f3 <- function(x) 0*x
f <- f0(x0) + f1(x1) + f2(x2)
g <- (f-5)/3
g <- binomial()$linkinv(g)
y <- rbinom(g,1,g)
my.data <- as.data.frame(cbind(y, x0, x1, x2, x3))

# Estimate model, present a summary and a plot of the results:
user.prompt()
z.out <- zelig(y~s(x0)+s(x1)+s(x2)+s(x3), model="logit.gam", data=my.data)
user.prompt()
summary(z.out)
user.prompt()
plot(z.out, pages=1)

## Plot model components with truth overlaid in red
op <- par(mfrow=c(2,2))
for (k in 1:4) {
  plot(z.out,residuals=TRUE,select=k)
  xx <- sort(eval(parse(text=paste("x",k-1,sep=""))))
  ff <- eval(parse(text=paste("f",k-1,"(xx)",sep="")))
  lines(xx,(ff-mean(ff))/3,col=2)   }
par(op)

# Set explanatory variables to their default (mean/mode) values, with
# high (80th percentile) and low (20th percentile) values:
user.prompt()
x.high <- setx(z.out, x3= quantile(my.data$x3, 0.8))
x.low  <- setx(z.out, x3 = quantile(my.data$x3, 0.2))

# Generate first differences for the effect of high versus low x3 on y:
user.prompt()
s.out <- sim(z.out, x=x.high, x1=x.low)
user.prompt()
summary(s.out)

# Generate a second set of fitted values and a plot:
user.prompt()
plot(s.out)

## We can also run ANOVA on the model output
user.prompt()
anova(z.out)

## We can pull out and compare AICs too...
user.prompt()
z1.out <- zelig(y~s(x0)+s(x1)+s(x2), model="logit.gam", data=my.data)
z2.out <- zelig(y~s(x1)+s(x2), model="logit.gam", data=my.data)
AIC(z.out,z1.out,z2.out)
