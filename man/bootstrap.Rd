\name{bootstrap}
\alias{bootstrap}
\title{Generic Method for ``bootstrap''}
\usage{
  bootstrap(obj)
}
\arguments{
  \item{obj}{a fitted model object that will be used to
  produce boot-strapped parameters. This object usually
  inherits the class ``glm'' or ``lm'' object}
}
\value{
  a list with the ``alpha'' and ``beta'' slots set. Note
  that ``alpha'' corresponds to ancillary parameters and
  ``beta'' corresponds to systematic components of the
  model
}
\description{
  This method is intended to be overried by statistical
  models that would like to support statistical
  bootstrapping.
}
\note{
  This method has private memory storage and can reference
  the objects: ``.fitted'', ``.data'', ``.call'', ``.env'',
  despite having no declaration in the argument list.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

