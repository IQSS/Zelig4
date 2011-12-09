\name{model.matrix.parseFormula}
\alias{model.matrix.parseFormula}
\title{Construct Design Matrix from a Parsed, Zelig-style Formula...}
\usage{\method{model.matrix}{parseFormula}(object, data = NULL, ...)}
\description{Construct Design Matrix from a Parsed, Zelig-style Formula}
\details{This method constructs a design matrix from a Zelig-style formula. This
matrix is commonly used in statistical simulation, and will likely be
relevent as the relevant form of a \code{setx} object.}
\note{This method is primarily used by the \code{setx} function.}
\value{a "model.matrix" specifying information relevant to a statistical
model}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{object}{a "parseFormula" object}
\item{data}{a "data.frame"}
\item{...}{ignored parameters}}
