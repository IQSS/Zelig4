\name{is.zelig.compliant}
\alias{is.zelig.compliant}
\title{Whether a R-Package Contains a 'Yes' in its DESCRIPTION File's 'Zelig' Field}
\usage{
  is.zelig.compliant(package = "")
}
\arguments{
  \item{package}{a character-string specifying an installed
  R-package}
}
\value{
  whether the package's DESCRIPTION file specifies
  Zelig-compliancy
}
\description{
  Whether a R-Package Contains a 'Yes' in its DESCRIPTION
  File's 'Zelig' Field
}
\note{
  This package was used internally to determine whether an
  R-package is Zelig compliant, but is now likely
  deprecated.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}
\seealso{
  is.zelig.package
}

