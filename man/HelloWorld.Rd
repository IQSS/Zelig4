\name{HelloWorld}
\alias{HelloWorld}
\title{HelloWorld}
\description{This document specifies information concerning the simple "Hello, World" 
Zelig model.}
\note{Run \code{example(HelloWorld)} to create and run the Zelig
"Hello, World" model. This gives a step-by-step view of how Zelig models
are made.}
\examples{
## Load data
data(turnout)

## Define External Method
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

\dontshow{user.prompt()}

describe.HelloZelig <- function (...) {
  list(author = "You", text='A "Hello, World!" Model')
}

\dontshow{user.prompt()}

zelig2HelloZelig <- function (formula, ..., data) {
  list(                                            
       .function = "HelloWorldMethod",
       formula = formula,
       data = data
       )
}

\dontshow{user.prompt()}

param.HelloZelig <- function(obj, num=1000, ...) {
  list(
       alpha = .5,
       linkinv = NULL
       )
}

\dontshow{user.prompt()}

qi.HelloZelig <- function(obj, x=NULL, x1=NULL, y=NULL, num=1000, param=NULL) {

  possibilities <- c('Hello', 'World')
  success.prob <- alpha(param)
  
  
  sims <- rbinom(n=num, 1, success.prob) + 1
  pv <- possibilities[sims]

  list(
       "Predicted Value: E(Y|X)" = pv
       )
}

\dontshow{user.prompt()}

## Run Zelig Functions
z <- zelig(~ 1, model="HelloZelig", data=turnout)
x <- setx(z)
s <- sim(z)

## Display Fictional Summary
summary(s)


}
