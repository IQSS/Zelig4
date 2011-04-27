\name{as.parameters}
\alias{as.parameters}
\title{Generic Method for Converting Objects into 'parameters'...}
\usage{as.parameters(params, ...)}
\description{Generic Method for Converting Objects into 'parameters'}
\details{Converts list-style objects into Parameter lists primarily used by the 'qi'
methods. These list-style objects may contain keys specifying: 'link' (the 
link function of a statistical model), 'linkinv' (the inverse-link
'family' (a object of 'family' class used to specify the model's
classification), 'alpha' (a vector of ancillary parameters, and 'simulations'
(a vector of simulated draws from the model's underlying distribution.}
\note{Only three scenarios may exist - converting 'parameters' to
'parameters', 'list' to 'parameters', and vectors to 'parameters'. The
third in particular is needed only for backwards compatibility, and support
will likely be deprecated.

Furthermore, this function should be exlusively used implicitly and
by Zelig.}
\value{an object of type `parameters'}
\seealso{as.parameters.list as.parameters.parameters, as.parameters.default}
\author{Matt Owen \email{mowen@ig.harvard.edu}}
\arguments{\item{params}{the object to be casted}
\item{...}{parameters reserved for future revisions}}
