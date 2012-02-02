\name{as.qi.list}
\alias{as.qi.list}
\title{list -> qi
This function has a lot of room to go wrong. It tries o detect whether the
zelig model is old-style or new-style (as of 4/4/2011). Eventually this
feature should be phased out.}
\usage{
  as.qi.list(s)
}
\arguments{
  \item{s}{a list}
}
\value{
  an object of type `qi'
}
\description{
  list -> qi This function has a lot of room to go wrong.
  It tries o detect whether the zelig model is old-style or
  new-style (as of 4/4/2011). Eventually this feature
  should be phased out.
}
\note{
  This method has peculiar behavior when the list contains
  only two elements. The crucial fix is to simply remove
  the portion of code which intentionally implements this
  perculiar behavior.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

