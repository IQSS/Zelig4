\name{tolmerFormat}
\alias{tolmerFormat}
\title{Convert a Formula into 'lmer' Representation from Reduced Form...}
\usage{tolmerFormat(f)}
\description{Convert a Formula into 'lmer' Representation from Reduced Form
Take a formula in its reducd from and return it as a 'lmer' representation
(from the lme4 package). This is basically removing the starting 'tag' from
each term.}
\value{the 'lmer' representation of 'f'}
\author{Ferdinand Alimadhi, Kosuke Imai, and Olivia Lau}
\arguments{\item{f}{a formula in reduced form}}
