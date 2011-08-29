\name{mi}
\alias{mi}
\title{Bundle Data-sets for Multiple Imputation...}
\usage{mi(...)}
\description{Bundle Data-sets for Multiple Imputation}
\details{This object prepares data-sets for processing with multiple imputation.}
\note{This function is largely identical to simply creating a list object,
with the exception that any unnamed data-sets are automatically labeled
via the \code{substitute} function}
\value{an \code{almost.mi} object, which contains the important internals
of a valid, useful \code{mi} object}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{...}{a set of \code{data.frame}'s}}
