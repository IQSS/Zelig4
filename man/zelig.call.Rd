\name{zelig.call}
\alias{zelig.call}
\title{Construct a Call to a Model Fitting Function...}
\usage{zelig.call(model, params, from.call)}
\description{Construct a Call to a Model Fitting Function
This function parses the parameters submitted by the 'zelig2' function and
constructs an object, useable by Zelig, to fit the model.}
\note{This function is exclusively used internally by Zelig.}
\value{a zelig.call object which is later used to call the model fitting
function}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{model}{character-string or function for the model to call}
\item{params}{list containing values that the user input}
\item{from.call}{the original Zelig call}}
