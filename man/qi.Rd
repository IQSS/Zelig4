\name{qi}
\alias{qi}
\title{Generic Method for Computing Quantities of Interest}
\usage{qi(obj, x, x1, y, num, param)}
\description{The \code{qi} function is used by developers to simulated quantities of
interest. This method, as a result, is the most significant method of any
Zelig statistical model.}
\value{a list of key-value pairs specifying pairing titles of
quantities of interest with their simulations}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\note{Run \code{example(qi)} to see a trivial version of}
\arguments{\item{obj}{a \code{zelig} object}
\item{x}{a \code{setx} object or NULL}
\item{x1}{an optional \code{setx} object}
\item{y}{this parameter is reserved for simulating average treatment effects,
though this feature is currentlysupported by only a
handful of models}
\item{num}{an integer specifying the number of simulations to compute}
\item{param}{a parameters object}}
\examples{qi.some.model <- function(obj, x=NULL, x1=NULL, y=NULL, param=NULL) {
list(
"Expected Values: E(Y|X)" = NA,
"Predicted Values: Y|X"   = NA
)
}}
