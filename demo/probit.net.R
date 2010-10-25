## Example Probit Model

## Load sample data
## Estimate the model
## Summarize the results
data(friendship)
z.out <- zelig(friends ~ advice + prestige + perpower, model="probit.net", data=friendship)
summary(z.out)
user.prompt()

## Estimating the risk difference (and risk ratio) between low personal power 
## (25th percentile) and high personal power (75th percentile) while all the 
## other variables are held at their default values. 
x.high <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.75))
x.low <- setx(z.out, perpower = quantile(friendship$perpower, prob=0.25))
user.prompt()

## Simulate quantities of interest
## Summarize the results of the simulation
## Plot those results
s.out <- sim(z.out, x = x.high, x1 = x.low)
summary(s.out)
plot(s.out)
