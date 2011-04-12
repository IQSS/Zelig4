\name{zelig.skeleton}
\alias{zelig.skeleton}
\title{Creates a skeleton zelig package...}
\usage{zelig.skeleton(pkg, models=c(), author=c(), environment=.GlobalEnv,
    path=".", force=FALSE, email="", depends=c())}
\description{Creates a skeleton zelig package}
\value{nothing}
\author{Matt Owen \email{mowen@iq.harvard.edu}}
\arguments{\item{pkg}{a character-string specifying the name of the Zelig package}
\item{models}{a vector of strings specifying models to be included in the package}
\item{author}{a vector of strings specifying contributors to the package}
\item{environment}{- ignored -}
\item{path}{a character-string specifying the path to the package}
\item{force}{a logical specifying whether to overwrite files and create necessary directories}
\item{email}{a string specifying the email address of the package's maintainer}
\item{depends}{a vector of strings specifying package dependencies}}
