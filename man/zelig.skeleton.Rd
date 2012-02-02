\name{zelig.skeleton}
\alias{zelig.skeleton}
\title{Creates a Skeleton for a New Zelig package}
\usage{
  zelig.skeleton(pkg, models = c(),
    author = "UNKNOWN AUTHOR", path = ".", force = FALSE,
    email = "maintainer@software-project.org",
    depends = c(), ..., .gitignore = TRUE,
    .Rbuildignore = TRUE)
}
\arguments{
  \item{pkg}{a character-string specifying the name of the
  Zelig package}

  \item{models}{a vector of strings specifying models to be
  included in the package}

  \item{author}{a vector of strings specifying contributors
  to the package}

  \item{path}{a character-string specifying the path to the
  package}

  \item{force}{a logical specifying whether to overwrite
  files and create necessary directories}

  \item{email}{a string specifying the email address of the
  package's maintainer}

  \item{depends}{a vector of strings specifying package
  dependencies}

  \item{...}{ignored parameters}

  \item{.gitignore}{a logical specifying whether to include
  a copy of a simple \code{.gitignore} in the appropriate
  folders (\code{inst/doc} and the package root}

  \item{.Rbuildignore}{a logical specifying whether to
  include a copy of a simple \code{.Rbuildignore} in the
  appropriate folders (\code{inst/doc} and the package
  root}
}
\value{
  nothing
}
\description{
  'zelig.skeleton' generates the necessary files used to
  create a Zelig package. Based on (and using) R's
  'package.skeleton' it removes some of the monotony of
  building statistical packages. In particular,
  'zelig.skeleton' produces templates for the
  \code{zelig2}, \code{describe}, \code{param}, and
  \code{qi} methods. For more information about creating
  these files on an individual basis, please refer to the
  tech manuals, which are available by typing:
  \code{?zelig2}, \code{?param}, or \code{?qi}.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

