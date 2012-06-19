\name{bootfn.default}
\alias{bootfn.default}
\title{Default Boot-strapping procedure}
\usage{
  bootfn.default(data, i, object, bootstrapfn = NULL, num,
    ...)
}
\arguments{
  \item{data}{a data.frame}

  \item{i}{an integer or chacter-string specifying the
  index of the row to be used in the bootstrapping
  procedure.}

  \item{object}{the fitted model object}

  \item{bootstrapfn}{a function used to bootstrap the
  object}

  \item{num}{an integer specifying the number of samples to
  simulate}

  \item{...}{unspecified parameters}
}
\value{
  a list of paramters
}
\description{
  The default procedure for extracting bootstrap
  information. Note that this method re-fits the data and
  resamples the data frequently. This is a good candidate
  for fixing-up.
}

