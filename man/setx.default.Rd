\name{setx.default}
\alias{setx.default}
\title{Set explanatory variables...}
\usage{setx.default(obj, fn, data, cond=FALSE, ...)}
\description{Set explanatory variables}
\value{a 'setx' object}
\author{Matt Owen \email{mowen@iq.harvard.edu}, Kosuke Imai, and Olivia Lau}
\arguments{\item{obj}{a 'zelig' object}
\item{fn}{a list of key-value pairs specifying which function apply to
columns of the keys data-types}
\item{data}{a data.frame}
\item{cond}{ignored}
\item{...}{parameters specifying what to explicitly set each column as. This
is used to produce counterfactuals}}
