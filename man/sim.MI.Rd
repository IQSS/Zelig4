\name{sim.MI}
\alias{sim.MI}
\title{Simulate Multiply Imputed Data...}
\usage{\method{sim}{MI}(obj, x=NULL, x1=NULL, y=NULL, num=1000)}
\description{Simulate Multiply Imputed Data}
\value{a 'sim.MI' with simulated quantities of interest for each fitted
contained by 'obj'}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\seealso{\link{sim}}
\arguments{\item{obj}{a 'zelig.MI' object containing several fits for two or more 
subsetted data-frames}
\item{x}{a 'setx.mi' object containing explanatory variables for each
fitted model}
\item{x1}{a 'setx.mi' object containing explanatory variables for each
fitted model}
\item{y}{this feature is currently unimplemented}
\item{num}{an integer specifying the number of simulations to compute}}
