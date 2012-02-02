\name{depends.on.zelig}
\alias{depends.on.zelig}
\title{Whether a Statistical Package Depends on the Zelig Software Suite}
\usage{
  depends.on.zelig(package = "")
}
\arguments{
  \item{package}{a character-string representing a package
  name}
}
\value{
  whether the package lists Zelig as a dependency in its
  DESCRIPTION
}
\description{
  Whether a Statistical Package Depends on the Zelig
  Software Suite
}
\note{
  This function is used primarily internally to determine
  whether a a package is contributing a function to the
  Zelig software suite
}

