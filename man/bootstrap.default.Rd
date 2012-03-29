\name{bootstrap.default}
\alias{bootstrap.default}
\title{Produce Boot-strapped Parameters for a Statistical Model}
\usage{
  bootstrap.default(obj)
}
\arguments{
  \item{obj}{a fitted model object. This is typically of
  type ``glm'' or ``lm''}
}
\value{
  a list with the ``alpha'' and ``beta'' slots set
}
\description{
  This method is a fallback for bootstrapping models that
  do not have a defined ``bootstrap'' method. For most
  models, this default is sufficient, so long as the model
  follows the usual convention that ``coef(obj)'' returns
  the systematic parameters of a fitted model.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

