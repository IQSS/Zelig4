\name{makeZeligObject}
\alias{makeZeligObject}
\title{Make an Individual Zelig Object}
\usage{
  makeZeligObject(object, model, call, zelig_call, data,
    label, env, package.name = NULL)
}
\arguments{
  \item{object}{a fitted statistical model}

  \item{model}{a character-string specifying the name of
  the model}

  \item{call}{The call that produced the fitted model}

  \item{zelig_call}{The call made to the original zelig
  function}

  \item{data}{the data.frame used to fit the model}

  \item{label}{a character-string or symbol used as a
  human-readable label for the data-set}

  \item{env}{an environment variable that contains all
  variables to evaluate the call ``zelig_call''}
}
\value{
  ....
}
\description{
  Returns a ``zelig'' object with the proper specifications
}

