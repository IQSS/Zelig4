## Load data
data(turnout)

# The following demo is a step-by-step instruction guide on building a Zelig
# model. For the most part, the steps have been simplified, and the model
# itself is simply written to show broad ideas, rather than the specifics
# of developing a fully functioning statistical model

user.prompt("Press <return> to Read about External Methods")

# Step 1: Creating and Using External Methods (optional)
# ======================================================
# Create a model to be used to be used by the Zelig function. This method
# should be designed with the singular purpose of fitting a statistical model.
# That is, it should analyze a data-set given several parameters
#
# For the most part, this step is optional, as quite often R contains builtin
# functions for doing these kinds of analyses. Regardless, this step is kept
# here for completeness.
#
# The foreign model, in its simplest form, has only one of two requirements,
# either:
#   1. The model contains a slot labeled "formula", or
#   2. There is a "formula" method defined for objects of this class

user.prompt("Press <return> to Continue to Step 1")



HelloWorldMethod <- function(formula, verbose=TRUE, data) {
  if (verbose) {
    print.form <- paste(as.character(formula), collapse=" ")
    print.data <- as.character(substitute(data))
  cat("Hello, Zelig!\n")
  }

  x <- list(formula = formula)
  class(x) <- "HelloWorld"
  x
}

user.prompt("Press <return> to Read about Describing Zelig Models")



# Step 2: Describing Zelig Models (optional)
# ==========================================
# Describing the model is an optional, though important step if the developer
# would like to be correctly cited in scholarly documents. In its most basic
# form, it is simply a list specifying "authors", "text" as the title-text, 
# and and publication year.

user.prompt("Press <return> to Continute to Step 2")

describe.hello <- function (...) {
  list(authors = "You", text='A "Hello, World!" Model')
}

user.prompt("Press <return> to Read about zelig2 Functions")

# Step 3: Interfacing between the External Model and Zelig (crucial)
# ==================================================================
# The 'zegli2' function of a model is named in the style the model's name
# appended to "zelig2". This informs Zelig that a model by the appropriate
# name exists. In this demo, "hello" is the model's name, and, as such,
# the zelig2 function is named "zelig2hello".
#
# In the upcoming example, please note that the parameters of the external
# method "HelloWorldMethod" are all included within the list that is being
# returned from the "zelig2hello" function.
#
# In general, all "zelig2" functions follow this format. For more detailed
# information concerning "zelig2" functions, type:
#    ?zelig2
#
# within an R session.

user.prompt("Press <return> to See an Example of a \"zelig2\" Method")



zelig2hello <- function (formula, ..., data) {
  list(                                            
       .function = "HelloWorldMethod",
       formula = formula,
       data = data
       )
}

user.prompt('Press <return> to Read about the "param" Functions')

# Step 4: Simulating Parameters
# =============================
# The "param" function of a Zelig model is written by concatenating "param."
# with the model's name. In the ongoing example, the "hello" model will have
# a param function named "param.hello". 
#
# The retun value of a "param" function is a list optionally containing the 
# values: simulations, alpha, link, linkinv, and family. For more detailed
# concerning writing "param" functions, type:
#   ?param
#
# within an R session.

user.prompt('Press <return> to See an Example "param" Function')

param.hello <- function(obj, num=1000, ...) {
  list(
       simulations = rbinom(n=num, 1, .5),
       alpha = .5,
       linkinv = NULL
       )
}

user.prompt('Press <return> to Read about "qi" Methods')


# Step 5: Simulating Quantities of Interest
# =========================================
# The "qi" method of a Zelig model is written by concatentating "qi." with the
# model's name. In the ongoing example, the "hello" model will have a qi method
# named "qi.hello".
#
# The return-value of a qi method is a list pairing titles of quantities of
# interest and their simulations. For example, a model that computes
# "Expected Values" will have a return value:
#    list("Expected Values" = ev)
#
# where 'ev' is a variable containing the simulated expected value. For more 
# detailed information concerning writing 'qi' methods, type:
#   ?qi
#
# within an R session.

user.prompt('Press <return> to See and Example "qi" Method')

qi.hello <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  possibilities <- c('Hello', 'World')
  success.prob <- alpha(param)
  
  
  sims <- rbinom(n=num, 1, success.prob) + 1
  pv <- possibilities[sims]

  list(
       "Predicted Value: E(Y|X)" = pv
       )
}

user.prompt('Press <return> to Read More about Zelig')

# More Information about Zelig
# ============================
# That's it! Now that the zelig2, qi, and param methods are defined, Zelig can
# run the "hello" model. For more detailed information concerning the Zelig
# package, visit:
#   http://gking.harvard.edu/zelig
#
# or type:
#   ?Zelig
#
# within an R-session

user.prompt('Press <return> to see the results of the "hello" model')

## Run Zelig Functions
z <- zelig(~ 1, model="hello", data=turnout)
x <- setx(z)
s <- sim(z)

## Display Fictional Summary
summary(s)
