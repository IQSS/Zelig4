\name{loadDependencies}

\alias{loadDependencies}
\alias{load.dependencies}

\title{Load External Dependencies Safely and Dynamically}

\usage{
  loadDependencies(..., character.only = FALSE)
}

\arguments{
  \item{\ldots}{
    A collection of packages to load. If ``character.only''=FALSE, these can be
    entered symbolically (e.g. loadDependencies(MASS)). Otherwise, these
    arguments are character-strings.
  }

  \item{character.only}{
    A boolean specifying whether the arguments are strictly character-strings.
  }
}

\value{
  TRUE (invisibly) if successful. Otherwise the script is stopped.
}

\description{
  ``loadDependencies'' is a helper function for loading external dependencies
  at runtime.
}

\note{
  This is used by Zelig developers to dynamically load ``dependent'' pacakges at
  runtime.
}
