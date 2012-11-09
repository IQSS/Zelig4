#####  Example 1: User has Existing Sample Weights #####

# Attach sample data and variable names:  
data(api)

# In this example, we will estimate a model using 
# the percentages of students who receive subsidized 
# lunch and an indicator for whether schooling is 
# year-round to predict California public schools' 
# academic performance index scores:

z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",  
  weights=~pw, data = apistrat)
summary(z.out1)

# Set explanatory variables to their default (mean/mode) values, and set
# a high (80th percentile) and low (20th percentile) value for "meals,"
# the percentage of students who receive subsidized meals:

x.low <- setx(z.out1, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out1, meals= quantile(apistrat$meals, 0.8))

# Generate first differences for the effect of high versus low "meals" 
# on academic performance:

s.out1 <- sim(z.out1, x=x.high, x1=x.low)
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

z.out2 <- zelig(api00 ~ meals + yr.rnd, model = "normal.survey",  
  strata=~stype, fpc=~fpc, data = apistrat)
summary(z.out2)

# Note that these results are identical to the results obtained
# when pre-existing sampling weights were used.  When sampling 
# weights are omitted, Zelig estimates them automatically for 
# "normal.survey" models based on the user-defined description 
# of sampling designs.  If no description is present, the default 
# assumption is equal probability sampling.
# 
# setx() and sim() can then be run on z.out2 in the same fashion 
# described in Example 1.



#####  Example 3: User has Replicate Weights #####

# Load data for a model using the number of out-of-hospital
# cardiac arrests to predict the number of patients who arrive 
# alive in hospitals: 

data(scd)

# Create four Balanced Repeated Replicate (BRR) weights:

BRRrep<-2*cbind(c(1,0,1,0,1,0), c(1,0,0,1,0,1), c(0,1,1,0,0,1),
c(0,1,0,1,1,0))

# Estimate the model using Zelig:

z.out3 <- zelig(formula=alive ~ arrests , model = "normal.survey", 
  repweights=BRRrep, type="BRR", data=scd, na.action=NULL)
summary(z.out3)

# Set the explanatory variable at its minimum and maximum 

x.min <- setx(z.out3, arrests = min(scd$alive))
x.max <- setx(z.out3, arrests = max(scd$alive))

# Generate first differences for the effect of the minimum
# versus the maximum number of cardiac arrests on the number
# of people who arrive alive:

s.out3 <- sim(z.out3, x=x.max, x1=x.min)
summary(s.out3)

# Generate a second set of fitted values and a plot:
plot(s.out3)

#### The user should also refer to the normal model demo, since ####
#### normal.survey models can take many of the same options as  ####
#### normal models. 		 					    ####

