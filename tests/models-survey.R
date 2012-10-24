library(Zelig)

data(api, package = 'survey')
data(scd, package = 'survey')

# gamma.survey (1 of 3)
# gamma.survey (1 of 3)
# gamma.survey (1 of 3)

# TEST 1
z.out1 <- zelig(api00 ~ meals + yr.rnd,
                model   = 'gamma.survey',  
                weights = ~ pw,
                data    = apistrat
                )
summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.high, x1=x.low)

plot(s.out1)

# gamma.survey (2 of 3)
# gamma.survey (2 of 3)
# gamma.survey (2 of 3)

z.out2 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "gamma.survey",  
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)

jk1reps <- jk1weights(psu=apistrat$dnum)

# gamma.survey (2 of 3)
# gamma.survey (2 of 3)
# gamma.survey (2 of 3)

z.out3 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "gamma.survey", 
		data = apistrat,
                repweights=jk1reps$weights,
		type="JK1"
                )

summary(z.out3)

x.low <- setx(z.out3, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out3, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out3 <- sim(z.out3, x=x.high, x1=x.low)


plot(s.out3)

# logit.survey (1 of 3)
# logit.survey (1 of 3)
# logit.survey (1 of 3)

data(api, package="survey")


# TEST 1
z.out1 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "logit.survey",
                weights=~pw,
                data = apistrat
                )
summary(z.out1)


x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

# 
x.low
x.high

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

plot(s.out1)

# logit.survey (2 of 3)
# logit.survey (2 of 3)
# logit.survey (2 of 3)

z.out2 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "logit.survey",
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )
summary(z.out2)

# logit.survey (3 of 3)
# logit.survey (3 of 3)
# logit.survey (3 of 3)

data(scd)

scd$sued <- as.vector(c(0,0,0,1,1,1))

BRRrep<-2 * cbind(
                  c(1,0,1,0,1,0),
                  c(1,0,0,1,0,1),
                  c(0,1,1,0,0,1),
                  c(0,1,0,1,1,0)
                  )


z.out3 <- zelig(
                formula=sued ~ arrests + alive,
                model = "logit.survey",
                repweights=BRRrep,
                type="BRR",
                data=scd
                )

summary(z.out3)

x.low <- setx(z.out3, arrests = quantile(scd$arrests, .2))
x.high <- setx(z.out3, arrests = quantile(scd$arrests,.8))

s.out3 <- sim(z.out3, x=x.high, x1=x.low)

# normal.survey (1 of 3)
# normal.survey (1 of 3)
# normal.survey (1 of 3)

z.out1 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "normal.survey",  
                weights=~pw,
                data = apistrat
                )

summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.high, x1=x.low)

plot(s.out1)

z.out2 <- zelig(
                api00 ~ meals + yr.rnd,
                model = "normal.survey",  
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)

# normal.survey (2 of 3)
# normal.survey (2 of 3)
# normal.survey (2 of 3)

BRRrep<-2 * cbind(
                  c(1,0,1,0,1,0),
                  c(1,0,0,1,0,1),
                  c(0,1,1,0,0,1),
                  c(0,1,0,1,1,0)
                  )

z.out3 <- zelig(
                formula=alive ~ arrests,
                model = "normal.survey", 
                repweights=BRRrep,
                type="BRR",
                data=scd,
                na.action=NULL
                )

summary(z.out3)

x.min <- setx(z.out3, arrests = min(scd$alive))
x.max <- setx(z.out3, arrests = max(scd$alive))

x.min
x.max

s.out3 <- sim(z.out3, x=x.max, x1=x.min)

plot(s.out3)

data(api, package="survey")

# TEST 1
z.out1 <- zelig(enroll ~ api99 + yr.rnd , model = "poisson.survey", data = apistrat)
summary(z.out1)

x.low <- setx(z.out1, api00= quantile(apistrat$api00, 0.2))
x.high <- setx(z.out1, api00= quantile(apistrat$api00, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.low, x1=x.high)

plot(s.out1)


# TEST 2
z.out2 <- zelig(
                enroll ~ api99 + yr.rnd,
                model = "poisson.survey",
                data = apistrat, 
                strata=~stype,
                fpc=~fpc
                )

summary(z.out2)

data(scd, package="survey")

BRRrep<-2*cbind(
                c(1,0,1,0,1,0),
                c(1,0,0,1,0,1),
                c(0,1,1,0,0,1),
                c(0,1,0,1,1,0)
                )

z.out3 <- zelig(
                alive ~ arrests,
                model = "poisson.survey", 
                repweights=BRRrep,
                type="BRR",
                data=scd
                )

summary(z.out3)

x.low <- setx(z.out3, arrests = quantile(scd$arrests, .2))
x.high <- setx(z.out3, arrests = quantile(scd$arrests,.8))

x.low
x.high

s.out3 <- sim(z.out3, x=x.high, x1=x.low)

plot(s.out3)

data(api, package="survey")

z.out1 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "probit.survey",
                weights=~pw,
                data = apistrat
                )

summary(z.out1)

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

x.low
x.high

s.out1 <- sim(z.out1, x=x.low, x1=x.high)


plot(s.out1)


# TEST 2
z.out2 <- zelig(
                yr.rnd ~ meals + mobility,
                model = "probit.survey",
                strata=~stype,
                fpc=~fpc,
                data = apistrat
                )

summary(z.out2)


data(scd)

scd$sued <- as.vector(c(0,0,0,1,1,1))

BRRrep<-2*cbind(
                c(1,0,1,0,1,0),
                c(1,0,0,1,0,1),
                c(0,1,1,0,0,1),
                c(0,1,0,1,1,0)
                )

z.out3 <- zelig(
                formula=sued ~ arrests + alive,
                model = "probit.survey", 
                repweights=BRRrep,
                type="BRR",
                data=scd
                )

summary(z.out3)

x.low <- setx(z.out3, arrests = quantile(scd$arrests, .2))
x.high <- setx(z.out3, arrests = quantile(scd$arrests,.8))

x.low
x.high

s.out3 <- sim(z.out3, x=x.high, x1=x.low)
