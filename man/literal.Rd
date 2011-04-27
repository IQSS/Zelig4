\name{literal}
\alias{literal}
\title{Force Evaluation of Parameter to the Call of the Foreign Model...}
\usage{literal(obj, type)}
\description{Force Evaluation of Parameter to the Call of the Foreign Model
Zelig typically stores return-values of 'zelig2' functions in an insulated
environment. This prevents this value from being stored, so that it can be
evaluated before being stored away.}
\note{This function is used by developers writing 'zelig2' functions. This
tool comes in particularly useful when using exotic 'formula' objects and
model frames.}
\value{a 'literal' object, which will be evaluated before being stored in}
\arguments{\item{obj}{any type of object}
\item{type}{this parameter is currently ignored by Zelig}}
