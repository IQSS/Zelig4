\name{repl.sim}
\alias{repl.sim}
\title{Method for Replicating Simulated Quantities of Interest...}
\usage{repl.sim(object, x, x1, y, num=1000, prev, bootstrap=FALSE, boot.fn,
    cond.data, ...)}
\description{Method for Replicating Simulated Quantities of Interest}
\value{a 'sim' object storing the replicated quantities of interest}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{object}{a 'zelig' object}
\item{x}{a 'setx' object}
\item{x1}{a secondary 'setx' object used to perform particular computations
of quantities of interest}
\item{y}{a parameter reserved for the computation of particular quantities of
interest (average treatment effects). Few models currently support this
parameter}
\item{num}{an integer specifying the number of simulations to compute}
\item{prev}{ignored}
\item{bootstrap}{ignored}
\item{boot.fn}{ignored}
\item{cond.data}{ignored}
\item{...}{special parameters which are reserved for future versions of Zelig}}
