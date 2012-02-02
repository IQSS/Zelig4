\name{ZeligListModels}
\alias{ZeligListModels}
\title{Get a Character-Vector of All Models with a 'zelig2' Function}
\usage{
  ZeligListModels(zelig.only = FALSE)
}
\arguments{
  \item{zelig.only}{a boolean specifying whether we want to
  search only the Zelig namespace}
}
\value{
  a character-vector of the Zelig models loaded on the
  user's machine
}
\description{
  Get a Character-Vector of All Models with a 'zelig2'
  Function
}
\note{
  In order for a Zelig model to either execute correctly or
  be listed as a legal Zelig model, the function name must
  be prefixed with 'zelig2'.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

