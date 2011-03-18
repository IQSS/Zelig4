\name{zelig.skeleton}
\alias{zelig.skeleton}

\title{Create a Skeleton for  Zelig Model}

\description{
  'zelig.skeleton' automates the procedure - directory structure,
  necessary R files, etc. - of building a Zelig model. This function
  is completely analgous to R's native 'package.skeleton' function.
}

\usage{
  zelig.skeleton(
                 pkg = "aZeligPackage",
                 models = c('model1', 'model2'),
                 environment = .GlobalEnv,
                 path = ".",
                 force = FALSE
                 )
}

\arguments{

  \item{pkg}{A character-string specifying the name of the Zelig package}

  \item{models}{A vector of character-strings specifying the names of
    the models to be included in the package}

  \item{environment}{Deprecated}

  \item{path}{A character-string specifying the directory in which to
    build the package}

  \item{force}{A logical specifying whether to overwrite files}

}

\details{
  'zelig.skeleton' wraps the 'package.skeleton' function and creates the 
  necessary files in the appropriate directories.

  In particular, 'zelig.skeleton' creates necessary R-files within the
  newly created package's 'R' folder.
}

\value{Nothing}

\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
zelig.skeleton(
               "someZeligModel",
               c("complex.model", "simple.model"),
               author = "An Author"
               )

# generates: R/qi.complex.model, R/qi.simple.model
#            R/param.complex.model, R/param.simple.model
#            R/zelig2complex.model, R/zelig2simple.model
}

\keyword{Zelig}
\keyword{package.skeleton}
\keyword{skeleton}
