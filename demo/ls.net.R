## Attaching the sample dataset:
data(sna.ex)

##### Example 1: Simple Example with First Differences

## Generating empirical estimates:
user.prompt()
z.out <- zelig(Var1 ~ Var2 + Var3 + Var4, model = "ls.net", data=sna.ex)
user.prompt()
## Viewing the regression output:
summary(z.out)

## Using setx to set explanatory variables to their default (mean/mode) values,
## with high (80th percentile) and low (20th percentile) for the second
## explanatory variable:
user.prompt()
x.high <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.8))
x.low <- setx(z.out, Var3 = quantile(sna.ex$Var3, 0.2))

## Simulating quantities of interest
user.prompt()
s.out <- sim(z.out, x=x.high, x1=x.low)
user.prompt()

## Summarizing the simulated quantities of interest:
summary(s.out)

## Diagnostic plot of the s.out:
user.prompt()
plot(s.out)

