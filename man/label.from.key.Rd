\name{label.from.key}
\alias{label.from.key}
\title{Get Labels from MI Key...}
\usage{label.from.key(key)}
\description{Get Labels from MI Key}
\details{This produced a human-readable label from a named character vector. This is
primarily used by the zelig function to give reasonable names to simulated
quantities of interest. That is, we can differentiate several imputed
data-frames from one another by which key we read.}
\note{This method is not exported, and is exclusively used internetally by
Zelig}
\value{a character-string}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{key}{a vectory of character-strings}}
