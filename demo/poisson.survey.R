#####  Example 1: User has Existing Sample Weights #####

# Attach sample data:  
data(api, package="survey")

# In this example, we will estimate a model using 
# each school's academic performance in 2000 and an
# indicator for year-round schools to predict the 
# number of students who enrolled in each California school.

z.out1 <- zelig(enroll ~ api99 + yr.rnd , model = "poisson.survey", data = apistrat)
summary(z.out1)

# Set explanatory variables to their default (mean/mode) values, and set
# a high (80th percentile) and low (20th percentile) value for the
# measure of academic performance, "api00":

x.low <- setx(z.out1, api00= quantile(apistrat$api00, 0.2))
x.high <- setx(z.out1, api00= quantile(apistrat$api00, 0.8))

# Generate first differences for the effect of high versus low "meals" 
# on the probability that a school will hold classes year round:

s.out1 <- sim(z.out1, x=x.low, x1=x.high)
summary(s.out1)

# Generate a second set of fitted values and a plot:

plot(s.out1)



####  Example 2: User has Details about Complex Survey Design  ####
####  (but not sample weights) 					   ####

# Suppose that the survey house that provided
# the dataset excluded probability weights 
# but made other details about the survey
# design available.  We can still estimate 
# a model without probability weights that takes
# instead variables that identify each the stratum
# and/or cluster from which each observation was
# selected and the size of the finite sample from
# which each observation was selected.

z.out2 <- zelig(enroll ~ api99 + yr.rnd , model = "poisson.survey", data = apistrat, 
  strata=~stype, fpc=~fpc)
summary(z.out2)

# The coefficient estimates from this model are identical to 
# point estimates in the previous example, but the standard errors
# are smaller.  When sampling weights are omitted, Zelig estimates 
# them automatically for "normal.survey" models based on the 
# user-defined description of sampling designs.  In addition, 
# when user-defined descriptions of the sampling design are 
# entered as inputs, variance estimates are better and standard
# errors are consequently smaller.
#
# setx() and sim() can then be run on z.out2 in the same fashion 
# described in Example 1.



#####  Example 3: User has Replicate Weights #####

# Load data for a model using the number of out-of-hospital
# cardiac arrests to predict the number of patients who arrive 
# alive in hospitals.

data(scd, package="survey")

# For the purpose of illustration, create four Balanced 
# Repeated Replicate (BRR) weights:

BRRrep<-2*cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1), c(0,1,1,0,0,1),
c(0,1,0,1,1,0))

# Estimate the model using Zelig:

z.out3 <- zelig(alive ~ arrests , model = "poisson.survey", 
  repweights=BRRrep, type="BRR", data=scd)
summary(z.out3)

# Set the explanatory variables at their means and set
# arrests at its 20th and 80th quartiles

x.low <- setx(z.out3, arrests = quantile(scd$arrests, .2))
x.high <- setx(z.out3, arrests = quantile(scd$arrests,.8))

# Generate first differences for the effect of the minimum
# versus the maximum number of individuals who arrive
# alive on the probability that a hospital will be sued:

s.out3 <- sim(z.out3, x=x.high, x1=x.low)
summary(s.out3)

# Generate a second set of fitted values and a plot:
plot(s.out3)

#### The user should also refer to the poisson model demo, since ####
#### poisson.survey models can take many of the same options as  ####
#### poisson models. 		 					     ####

