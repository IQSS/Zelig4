\name{attach.env}
\alias{attach.env}
\title{Attach Variables to a Function}
\usage{
  attach.env(f, env = NULL, ...)
}
\arguments{
  \item{f}{a function which will be modified}

  \item{env}{an environment variable which will be attached
  to the function being returned}

  \item{...}{arbitrary key-value paired parameters which
  will be assigned to the environment of the function being
  returned}
}
\value{
  the original function ``f'' with a different environment
  attached to it.
}
\description{
  Returns a function, specified by the user, with the
  variables of a specified environment attached. This, in
  essence, allows programmers to write functions that have
  forms of private memory. This makes the function behave
  similarly to an object.
}
\note{
  This function is used by Zelig to ensure that particular
  method calls - param, qi, bootstap - will contain the
  private variables: ``.fitted'', ``.model'', ``.call'' and
  ``.env'' which respectively contain the fitted model
  object, the name of the zelig model being invoked, the
  original call to the model-fitting function and the
  environment in which to call the function call.
}
\author{
  Matt Owen \email{mowen@iq.harvard.edu}
}

