\name{sim}
\alias{sim}
\title{Generic Method for Computing and Organizing Simulated Quantities of Interest...}
\usage{sim(obj, x, x1, y, num=1000, prev, bootstrap=F, boot.fn, cond.data,
    ...)}
\description{Generic Method for Computing and Organizing Simulated Quantities of Interest}
\value{an object of type "sim"}
\author{Matt Owen and Olivia Lau and Kosuke Imai \email{mowen@iq.harvard.edu}}
\arguments{\item{obj}{a zelig object}
\item{x}{a setx object}
\item{x1}{a secondary setx object used to perform
particular computations of quantities of interest}
\item{y}{a parameter reserved for the computation of particular
quantities of interest (average treatment effects). Few
models currently support this parameter}
\item{num}{an integer specifying the number of simulations to compute}
\item{bootstrap}{unsupported}
\item{boot.fn}{unsupported}
\item{cond.data}{unsupported}}
