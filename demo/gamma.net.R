## Example Gamma Model

## Load sample data
## Estimate the model
## Summarize the results
data(friendship)
z.out <- zelig(per ~ perpower, LF="inverse", model="gamma.net", data=friendship)
summary(z.out)

## Estimating the risk difference (and risk ratio) between low personal power 
## (25th percentile) and high personal power (75th percentile) while all the 
## other variables are held at their default values. 
user.prompt()
x.low <- setx(z.out, numst2 = 0)
x.high <- setx(z.out, numst2 = 1)

## Simulate quantities of interest
## Summarize the results of the simulation
## Plot those results
s.out <- sim(z.out, x = x.low, x1 = x.high)
summary(s.out)
plot(s.out)

