\name{zelig2}
\alias{zelig2}
\title{Interface Between Zelig Models and External Functions}
\value{
  The main purpose of the \code{zelig2} function is to
  return a list of key-value pairs, specifying how Zelig
  should interface with the external method. This list has
  the following format:

  \item{\code{.function}}{specifies the name of the
  external method to be called by \code{zelig} function.
  Subsequent parameters, are called and evaluated as a
  function call to the function of the named string.}
  \item{\code{.hook}}{specifies the name of a hook function
  as a string. The hook function is only evaluated on zelig
  object once the external method fits the statistical
  model} \item{...}{any parameters aside from
  \code{.function} and \code{.hook} is as part of the
  function call to the external model}
}
\description{
  The \code{zelig2} function acts as a simple interface
  between a user's call to the \code{zelig} function and
  the zelig functions subsequent call to the pre-existing
  external model. The external model varies based on which
  model is being called.
}
\note{
  Writing \code{zelig2} functions is required of Zelig
  developers. In particular, \code{zelig2} functions act as
  an interface between external models (models not included
  in the Zelig package) and the \code{zelig} function which
  must use that model.

  \code{zelig2} is not an actual function. Rather,
}
\examples{
zelig2some.model <- function (formula, weights, verbose, ..., data) {
   list(
        .function = 'some.other.method',
        .hook = NULL,
        formula = formula,
        weights = 2 * weights,
        data = data
        )
 }

## This \\code{zelig2} function equates the following function call:
##  zelig(formula, weights = weights, verbose = TRUE, data = data, model="some.model")
##
## with:
##  some.other.method(formula = formula, weights = 2 * weights, data=data)

## Note that the 'verbose' parameter is ignored, since the
## 'zelig2some.model' does not include the 'verbose' parameter in its return
## value.
}

