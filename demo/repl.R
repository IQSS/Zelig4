## Setting up output to be replicated
data(turnout)
z.out <- zelig(vote ~ race + age, model = "logit", data = turnout)
x.out <- setx(z.out)
set.seed(12345)
s.out <- sim(z.out, x = x.out)
s.out$seed <- 12345
user.prompt()

## Saving replication files
save(turnout, z.out, s.out, file = "demo_replication.RData")
user.prompt()

## Replicating simulations assuming that the seed was saved
load("demo_replication.RData")
s.rep <- repl(s.out)
identical(s.out$qi, s.rep$qi)
user.prompt()

## Replicating simulations, with previously generated parameters
s.rep2 <- repl(s.out, prev = s.out$par)
identical(s.rep2$qi$ev, s.out$qi$ev)
user.prompt()

## Replicating analyses on original data, assumes that the 
##  data frame is in the workspace with the original name
z.rep <- repl(z.out)
identical(coef(z.rep), coef(z.out))
user.prompt()

## Replicating analyses on new data
z.alt <- repl(z.out, data = turnout[1:100,])

## Saving replication files
save(turnout, z.out, s.out, file = "demo_replication.RData")
user.prompt()

##  Cleaning up the directory
unlink("demo_replication.RData")
