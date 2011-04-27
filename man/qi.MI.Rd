\name{qi.MI}
\alias{qi.MI}
\title{Compute Quantities of Interest for Multiply-Imputed Zelig Models...}
\usage{qi.MI(obj, x, x1, y, num=1000, param)}
\description{Compute Quantities of Interest for Multiply-Imputed Zelig Models}
\value{a list of key-value pairs specifying pairing titles of
quantities of interest with their simulations}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{obj}{a 'zelig' object}
\item{x}{a 'setx' object or NULL}
\item{x1}{an optional 'setx' object}
\item{y}{this parameter is reserved for simulating average treatment effects,
though this feature is currentlysupported by only a handful of models}
\item{num}{an integer specifying the number of simulations to compute}
\item{param}{a parameters object}}
