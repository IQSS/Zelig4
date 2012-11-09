#####  Example 1: User has Existing Sample Weights #####

# Attach sample data and variable names:  
data(api)

# In this example, we will estimate a model using 
# the percentages of students who receive subsidized 
# lunch and an indicator for whether schooling is 
# year-round to predict California public schools' 
# academic performance index scores:

z.out1 <- zelig(api00 ~ meals + yr.rnd, model = "gamma.survey",  
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

z.out2 <- zelig(api00 ~ meals + yr.rnd, model = "gamma.survey",  
  strata=~stype, fpc=~fpc, data = apistrat)
summary(z.out2)

# Note that these results are identical to the results obtained
# when pre-existing sampling weights were used.  When sampling 
# weights are omitted, Zelig estimates them automatically for 
# "gamma.survey" models based on the user-defined description 
# of sampling designs.  If no description is present, the default 
# assumption is equal probability sampling.
# 
# setx() and sim() can then be run on z.out2 in the same fashion 
# described in Example 1.



#####  Example 3: User has Replicate Weights #####

# Suppose that the survey house that published 
# these data withheld details about the survey 
# design and instead published replication weights

# For the purpose of illustration, create a set of
# jk1 replicate weights

jk1reps <- jk1weights(psu=apistrat$dnum)

# Estimate the model regressing api00 on the "meals" 
# "yr.rnd" variables. 

z.out3 <- zelig(api00 ~ meals + yr.rnd, model = "gamma.survey", 
		data = apistrat, repweights=jk1reps$weights,
		type="JK1")
summary(z.out3)

# Set the explanatory variable "meals" at high and low values

x.low <- setx(z.out3, meals= quantile(apistrat$meals, 0.2))
x.high <- setx(z.out3, meals= quantile(apistrat$meals, 0.8))

# Generate first differences for the effect of the high
# versus low concentrations of poverty on school performance

s.out3 <- sim(z.out3, x=x.high, x1=x.low)
summary(s.out3)

# Generate a second set of fitted values and a plot:

plot(s.out3)

#### The user should also refer to the gamma model demo, since  ####
#### gamma.survey models can take many of the same options as   ####
#### gamma models. 		 					    ####